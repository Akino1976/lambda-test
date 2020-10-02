# Postgresql & PgAdmin powered by compose


## Requirements:
* docker >= 17.12.0+
* docker-compose

## Install of psycopg2
Installed postgresql using **brew postgresql** and then used the following
command to install __psycopg2__
Need to have postgre installed, but running data from docker needs to have postgres closed globally, __brew services stop postgresql__
```bash
    env LDFLAGS='-L/usr/local/lib -L/usr/local/opt/openssl/lib -L/usr/local/opt/readline/lib' pip install psycopg2
```
## Quick Start
* Clone or download this repository
* Go inside of directory,  `cd compose-postgres`
* Run this command `docker-compose up -d`


## Environments
This Compose file contains the following environment variables:

* `POSTGRES_USER` the default value is **postgres**
* `POSTGRES_PASSWORD` the default value is **changeme**
* `PGADMIN_PORT` the default value is **5050**
* `PGADMIN_DEFAULT_EMAIL` the default value is **pgadmin4@pgadmin.org**
* `PGADMIN_DEFAULT_PASSWORD` the default value is **admin**

## Access to postgres:
* `localhost:5432`
* **Username:** postgres (as a default)
* **Password:** changeme (as a default)
* https://tferdinand.net/en/accelerate-the-test-of-your-lambda-functions-with-docker/
* https://github.com/khezen/compose-postgres
* https://naysan.ca/2020/05/09/pandas-to-postgresql-using-psycopg2-bulk-insert-performance-benchmark/

## Access to PgAdmin:
* **URL:** `http://localhost:5050`
* **Username:** pgadmin4@pgadmin.org (as a default)
* **Password:** admin (as a default)

## Add a new server in PgAdmin:
* **Host name/address** `postgres`
* **Port** `5432`
* **Username** as `POSTGRES_USER`, by default: `postgres`
* **Password** as `POSTGRES_PASSWORD`, by default `changeme`
