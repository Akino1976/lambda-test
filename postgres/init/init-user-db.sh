#!/bin/bash
set -e
DATA_BASE="doktor_test"
psql -U "$POSTGRES_USER" -tc "SELECT 1 FROM pg_database WHERE datname = '${DATA_BASE}'" | \
    grep -q 1 || \
    psql -U "$POSTGRES_USER" -c "CREATE DATABASE ${DATA_BASE} ENCODING = 'UTF8' CONNECTION LIMIT = -1;" 

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
    CREATE USER SA WITH PASSWORD 'Test-password';
    ALTER USER SA WITH SUPERUSER;
    GRANT ALL PRIVILEGES ON DATABASE ${DATA_BASE} TO SA;

    CREATE SCHEMA IF NOT EXISTS analytics;
    CREATE SCHEMA  IF NOT EXISTS base;
EOSQL
