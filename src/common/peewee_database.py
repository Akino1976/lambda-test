import logging
import time
import datetime

from peewee import *

from playhouse.reflection import generate_models, print_model, print_table_sql
from playhouse.migrate import *

logger = logging.getLogger(__name__)

import settings

CONNECTION_FORMAT = "postgresql+psycopg2://{user}:{password}@{host}:{port}/{database}"
PARAMETER_DICT = {
    "host": settings.PG_HOST,
    "port": 5432,
    "user": settings.PG_USER,
    "password": settings.PG_PASSWORD,
    "database": "doktor_test",
}


def _get_engine(**kwargs) -> PostgresqlDatabase:
    engine = PostgresqlDatabase(**kwargs)

    try:
        assert engine.connect()

    except Exception as exception:
        logger.error(f"Failed to get connection: {exception}")

        raise exception

    return engine


engine = _get_engine(**PARAMETER_DICT)


class BaseModel(Model):
    class Meta:
        database = engine
        schema = "base"


class Conversation(BaseModel):
    class Meta:
        table_name = "conversation"

    StateList = ["created", "opened", "closed", "archived"]
