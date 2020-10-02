#!/bin/bash
set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
    CREATE USER SA WITH PASSWORD 'Test-password';
    ALTER USER SA WITH SUPERUSER;
    CREATE DATABASE TEST_DB
        ENCODING = 'UTF8'
        CONNECTION LIMIT = -1;
    GRANT ALL PRIVILEGES ON DATABASE TEST_DB TO SA;

    CREATE TABLE IF NOT EXISTS conversation (
        id                              VARCHAR(50) NOT NULL,
        patient_id                      VARCHAR(50) NOT NULL,
        conversation_opened             VARCHAR(30) NULL,
        conversation_closed             VARCHAR(30) NULL,
        created_by_staff                CHAR(2) NULL,
        first_assigned                  VARCHAR(30) NULL,
        category                        VARCHAR(20) NULL,
        staff_1_role                    VARCHAR(20) NULL,
        staff_2_role                    VARCHAR(20) NULL,
        staff_3_role                    VARCHAR(20) NULL,
        staff_4_role                    VARCHAR(20) NULL,
        staff_5_role                    VARCHAR(20) NULL,
        staff_6_role                    VARCHAR(20) NULL,
        call_in_conversation            CHAR(2) NULL,
        call_time_in_seconds            INT NULL,
        sip_call_time_in_seconds        INT NULL,
        number_of_messages              INT NULL,
        number_of_bot_messages          INT NULL,
        number_of_client_messages       INT NULL,
        number_of_staff_messages        INT NULL,
        patient_cancelled               CHAR(2) NULL,
        highest_level_of_care           VARCHAR(20) NULL,
        staff_views                     INT NULL,
        mins_to_first_assign            INT NULL,
        rating_1                        VARCHAR(20) NULL,
        rating_2                        VARCHAR(20) NULL
    );

    ALTER TABLE conversation 
    ADD PRIMARY KEY (id);
    CREATE TABLE IF NOT EXISTS queue_process (
        id                  BIGINT NOT NULL,
        conversation        VARCHAR(50) NOT NULL,
        queue_name          VARCHAR(20) NULL,
        queue_start         VARCHAR(30) NULL,
        queue_end           VARCHAR(30) NULL,
        category            VARCHAR(30) NULL,
        mins_to_assign      VARCHAR(30) NULL,
        estimated_mins      VARCHAR(30) NULL
    );

    ALTER TABLE queue_process 
    ADD PRIMARY KEY (id);


    CREATE TABLE IF NOT EXISTS user_info (
        id              VARCHAR(50) NOT NULL,
        created         VARCHAR(30) NOT NULL,
        gender          CHAR(2) NULL,
        age_bin         CHAR(10) NULL,
        removed         CHAR(2) NULL,
        listed          CHAR(2) NULL,
        listed_since    VARCHAR(30) NULL,
        city            VARCHAR(30) NULL
    );
    ALTER TABLE user_info 
    ADD PRIMARY KEY (id);
    CREATE OR REPLACE VIEW view_conversation AS(
        SELECT USER_INFO.created::timestamp as user_created,
            USER_INFO.gender::boolean,
            USER_INFO.age_bin,
            USER_INFO.removed::boolean,
            USER_INFO.listed::boolean,
            USER_INFO.listed_since,
            USER_INFO.city,
            CONVERSATION_INFO.id,
            CONVERSATION_INFO.patient_id,
            CONVERSATION_INFO.conversation_opened::timestamp,
            CONVERSATION_INFO.conversation_closed::timestamp,
            CONVERSATION_INFO.created_by_staff::boolean,
            CONVERSATION_INFO.first_assigned,
            CONVERSATION_INFO.category,
            CONVERSATION_INFO.staff_1_role,
            CONVERSATION_INFO.staff_2_role,
            CONVERSATION_INFO.staff_3_role,
            CONVERSATION_INFO.staff_4_role,
            CONVERSATION_INFO.call_in_conversation::boolean,
            CONVERSATION_INFO.call_time_in_seconds,
            CONVERSATION_INFO.sip_call_time_in_seconds,
            CONVERSATION_INFO.number_of_messages,
            CONVERSATION_INFO.number_of_bot_messages,
            CONVERSATION_INFO.number_of_client_messages,
            CONVERSATION_INFO.number_of_staff_messages,
            CONVERSATION_INFO.patient_cancelled::boolean,
            CONVERSATION_INFO.highest_level_of_care,
            CONVERSATION_INFO.staff_views,
            CONVERSATION_INFO.mins_to_first_assign,
            CONVERSATION_INFO.rating_1
        FROM public.conversation AS CONVERSATION_INFO 
        LEFT JOIN public.user_info AS USER_INFO ON(
            CONVERSATION_INFO.patient_id = USER_INFO.id
        )
    );


EOSQL
