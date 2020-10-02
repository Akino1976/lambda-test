import os
import logging

from dotenv import load_dotenv
from logging.config import dictConfig

import yaml

BASE_DIRECTORY= os.path.realpath(
    os.path.join( os.path.dirname(__file__), '..' )
)
APP_DIRECOTRY = os.path.join(
    BASE_DIRECTORY,
    'src'
)
LOG_CONFIGURATION = os.path.join(
    BASE_DIRECTORY,
    'logging_config.yaml'
)

DATA_DIRECTORY = os.path.join(
    BASE_DIRECTORY,
    'DATA'
)

if not os.path.exists(LOG_CONFIGURATION):
    raise Exception(f'Confguration for log dont exists')

with open(LOG_CONFIGURATION, 'r') as stream:
    config = yaml.safe_load(stream.read())
    dictConfig(config)

ENVIRONMENT = os.getenv('ENVIRONMENT', 'test')
ignored_file_path = os.path.join(
    BASE_DIRECTORY,
    '.env'
)
dotenv_path = os.path.join(
    ignored_file_path,
    f'.{ENVIRONMENT}'
)
load_dotenv(dotenv_path)


PG_USER = os.getenv('PG_USER')
PG_PASSWORD = os.getenv('PG_PASSWORD')
PG_HOST = os.getenv('PG_HOST')
