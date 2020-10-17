import logging
import multiprocessing

from multiprocessing.connection import Connection, Pipe
from multiprocessing.context import Process
from typing import Any, Callable, Dict, List, Union

import common.utils as utils

logger = logging.getLogger(__name__)


def _get_chunked_list(
    dataset: List[Dict[str, Any]], tablename: str
) -> Union[Dict[str, Any], List[Dict[str, Any]]]:

    cpu_count = _get_available_cpu_count() - 1

    chunks = utils.get_chunk_list(dataset, cpu_count)

    return [dict(dataset=chunk, tablename=tablename) for chunk in chunks]


def _get_available_cpu_count() -> int:
    return multiprocessing.cpu_count()


def __chunk_wrapper(
    target: Callable[[Any], Any],
    target_kwargs: Dict[str, Any],
    target_name: str,
    connection: Connection,
    chunk_index: int,
    chunk_count: int,
):
    chunk_info = f"{chunk_index + 1}/{chunk_count}"

    try:
        logger.info(f"[{target_name}] Processing chunk {chunk_info}")
        response_data = target(**target_kwargs)
        connection.send(response_data)

        logger.info(f"[{target_name}] Successfully processed" f" chunk {chunk_info}")

    except Exception as error:
        logger.exception(f"[{target_name}] Error when processing chunk {chunk_info}")

        connection.send(error)

    finally:
        connection.close()


def run_in_parallel(
    target: Callable[[Any], Any],
    target_name: str,
    data_set: Dict[str, Any],
    table_name: str,
):

    return_values = []

    processes = []
    parent_connections = []

    chunked_kwargs = _get_chunked_list(dataset=data_set, tablename=table_name)

    logger.info(f"Starting up {len(chunked_kwargs)} process(es) for [{target_name}]")

    try:
        for chunk_index, target_kwargs in enumerate(chunked_kwargs):
            parent_connection, child_connection = Pipe()
            parent_connections.append(parent_connection)

            process = Process(
                target=__chunk_wrapper,
                kwargs=dict(
                    target=target,
                    target_kwargs=target_kwargs,
                    target_name=target_name,
                    connection=child_connection,
                    chunk_index=chunk_index,
                    chunk_count=len(chunked_kwargs),
                ),
                daemon=True,
            )

            process.start()
            processes.append(process)

        for connection in parent_connections:
            response_data = connection.recv()

            if isinstance(response_data, Exception):
                raise response_data

            return_values.append(response_data)

        for process in processes:
            process.join()

    except Exception as error:
        logger.exception(
            f"Error when running {len(processes)} process(es)" f" for {target_name}"
        )

        raise error

    finally:
        for process in processes:
            process.terminate()

    logger.info(
        f"Successfully executed {len(chunked_kwargs)} process(es) for [{target_name}]"
    )

    return return_values

