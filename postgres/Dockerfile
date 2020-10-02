FROM postgres:12

COPY ./init /docker-entrypoint-initdb.d/

RUN chmod -x /docker-entrypoint-initdb.d/init-user-db.sh

EXPOSE 5432