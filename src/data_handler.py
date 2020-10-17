import logging
import os

import psycopg2
import xlrd

import common.database as database
import common.csv_reader as csv_reader
import common.parallel_processing as parallel_processing

import settings


logger = logging.getLogger(__name__)


def main():
    logger.info(f"Starting insert to postgre....")

    DATA_MAPPING = {
        "conversation": "conversation-export.csv",
        "queue_process": "queue-export.csv",
        "user_info": "user-export.csv",
    }
    for table_name, file_name in DATA_MAPPING.items():
        file_path = os.path.join(settings.DATA_DIRECTORY, file_name)
        file_stream = open(file_path, "rb")
        body = file_stream.read()

        formatted_rows = csv_reader.parse_csv(body=body)

        total_insert = parallel_processing.run_in_parallel(
            target=database.insert_bulk_data,
            target_name="insert_bulk_data",
            data_set=formatted_rows,
            table_name=table_name,
        )

        logger.info(f"Inserted {sum(total_insert)} into {table_name}")


if __name__ == "__main__":
    main()

