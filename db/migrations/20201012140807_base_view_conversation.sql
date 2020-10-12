-- migrate:up
 CREATE OR REPLACE VIEW analytics.view_conversation AS(
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
        FROM base.conversation AS CONVERSATION_INFO 
        LEFT JOIN base.user_info AS USER_INFO ON(
            CONVERSATION_INFO.patient_id = USER_INFO.id
        )
    );

-- migrate:down

DROP VIEW IF EXISTS  analytics.view_conversation;