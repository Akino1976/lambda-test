import logging
import time
import datetime

from typing import (
    List,
    Dict,
    Any,
    Union,
    Tuple,
    Iterable
)

from sqlalchemy.orm import scoped_session, sessionmaker, load_only
from sqlalchemy.ext.automap import automap_base
from sqlalchemy import (
    MetaData,
    Table,
    create_engine,
    func,
    update,
    and_,
    or_,
    select,
    Numeric,
    String,
    bindparam
)
from sqlalchemy.engine import Engine
import pandas

logger = logging.getLogger(__name__)

import settings

CONNECTION_FORMAT = 'postgresql+psycopg2://{user}:{password}@{host}:{port}/{database}'
PARAMETER_DICT = {
    'host': settings.PG_HOST,
    'port': 5432,
    'user': settings.PG_USER,
    'password': settings.PG_PASSWORD,
    'database': 'TEST_DB'
}


def get_engine(endpoint_spec: Dict[str, str]=PARAMETER_DICT) -> Engine:
    engine = create_engine(
        CONNECTION_FORMAT.format(
            **endpoint_spec
        ),
         pool_pre_ping=True
    )

    try:
        assert engine.connect()

    except AssertionError as exception:
        logger.error(f'Failed to get connection: {exception}')

        raise exception

    return engine


def get_table(engine: Engine, tablename: str) -> Table:
    meta = MetaData()

    return Table(
        tablename,
        meta,
        autoload=True,
        autoload_with=engine
    )


def get_chunk(items: List[Dict[str, Any]], chunk_size: int=50) -> Iterable:
    for index in range(0, len(items), chunk_size):
        yield(items[index:(index + chunk_size)])


def insert_bulk_data(dataset: List[Dict[str, Any]],
                     tablename: str=None):
    try:
        metadata = MetaData()

        engine = get_engine()
        table = get_table(
            engine=engine, 
            tablename=tablename
        )
        metadata.reflect(engine, only=[tablename])

        Base = automap_base(metadata=metadata)
        Base.prepare()

        meta_table = Base.classes[tablename]

        session = scoped_session(sessionmaker())
        session.configure(
            bind=engine,
            autoflush=False,
            expire_on_commit=False
        )

        start_time = time.time()

        for chunk in get_chunk(dataset, chunk_size=2000):

            session.bulk_insert_mappings(
                meta_table,
                chunk
            )

            session.commit()

        total_time = time.time() - start_time
        session.remove()
        logger.info(
            f'Inserted {len(dataset)} records into the'
            f' table {tablename} in {round(total_time, 2)} seconds'
        )

    except Exception as error:
        logger.exception(f'Error in insert_data() for {tablename} {error}')

        raise error

    finally:
        engine.dispose()
