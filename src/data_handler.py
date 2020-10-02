import logging
import os

import pandas
import psycopg2
import xlrd

import common.database as database
import common.csv_reader as csv_reader

import settings

pandas.set_option('display.max_columns', None)
pandas.set_option('display.expand_frame_repr', False)
pandas.set_option('max_colwidth', 10)

logger = logging.getLogger(__name__)

def _pandas_insert():

    data_set = pandas.read_excel(
        os.path.join(
            settings.DATA_DIRECTORY,
            'clearing_201810_201910.xlsx'
        ),
        usecols='A:M',
        sheet_name=1,
        skiprows=9,
        nrows=100000
    )

    column_name = tuple(data_set.columns)
    data_set = [
        dict(
            zip(column_name, tuple(x))
        )
        for x in data_set.to_numpy()
    ]
    logger.info(f'Parsed {len(data_set)}')

    database.insert_bulk_data(
        dataset=data_set,
        tablename='settlment'
    )


def main():
    logger.info(f'Starting insert to postgre....')

    DATA_MAPPING = {
        'conversation': 'conversation-export.csv',
        'queue_process': 'queue-export.csv',
        'user_info': 'user_export.csv',
    }
    for table_name, file_name in DATA_MAPPING.items():
        file_path = os.path.join(
            settings.DATA_DIRECTORY,
            file_name
        )
        file_stream = open(file_path, 'rb')
        body = file_stream.read()

        formatted_rows = csv_reader.parse_csv(body=body)

        database.insert_bulk_data(
            dataset=formatted_rows,
            tablename=table_name
        )


if  __name__ == "__main__":
    main()

