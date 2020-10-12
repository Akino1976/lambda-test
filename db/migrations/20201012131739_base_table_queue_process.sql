-- migrate:up
    CREATE TABLE IF NOT EXISTS base.queue_process (
        id                  BIGINT NOT NULL,
        conversation        VARCHAR(50) NOT NULL,
        queue_name          VARCHAR(20) NULL,
        queue_start         VARCHAR(30) NULL,
        queue_end           VARCHAR(30) NULL,
        category            VARCHAR(30) NULL,
        mins_to_assign      VARCHAR(30) NULL,
        estimated_mins      VARCHAR(30) NULL,
        PRIMARY KEY (id, conversation)
    );


-- migrate:down

DROP TABLE base.queue_process 