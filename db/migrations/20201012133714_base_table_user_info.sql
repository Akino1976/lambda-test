-- migrate:up
    CREATE TABLE IF NOT EXISTS base.user_info (
        id              VARCHAR(50) PRIMARY KEY,
        created         VARCHAR(30) NOT NULL,
        gender          CHAR(2) NULL,
        age_bin         CHAR(10) NULL,
        removed         CHAR(2) NULL,
        listed          CHAR(2) NULL,
        listed_since    VARCHAR(30) NULL,
        city            VARCHAR(30) NULL
    );

-- migrate:down

DROP TABLE base.user_info

