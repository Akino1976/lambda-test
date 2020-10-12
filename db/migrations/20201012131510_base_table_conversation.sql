-- migrate:up
    CREATE TABLE IF NOT EXISTS base.conversation (
        id                              VARCHAR(50) PRIMARY KEY,
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


-- migrate:down

DROP TABLE base.conversation ;