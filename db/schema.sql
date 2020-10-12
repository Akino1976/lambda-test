SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: analytics; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA analytics;


--
-- Name: base; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA base;


--
-- Name: increment(integer); Type: FUNCTION; Schema: base; Owner: -
--

CREATE FUNCTION base.increment(i integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
    BEGIN
            RETURN i + 1;
    END;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: conversation; Type: TABLE; Schema: base; Owner: -
--

CREATE TABLE base.conversation (
    id character varying(50) NOT NULL,
    patient_id character varying(50) NOT NULL,
    conversation_opened character varying(30),
    conversation_closed character varying(30),
    created_by_staff character(2),
    first_assigned character varying(30),
    category character varying(20),
    staff_1_role character varying(20),
    staff_2_role character varying(20),
    staff_3_role character varying(20),
    staff_4_role character varying(20),
    staff_5_role character varying(20),
    staff_6_role character varying(20),
    call_in_conversation character(2),
    call_time_in_seconds integer,
    sip_call_time_in_seconds integer,
    number_of_messages integer,
    number_of_bot_messages integer,
    number_of_client_messages integer,
    number_of_staff_messages integer,
    patient_cancelled character(2),
    highest_level_of_care character varying(20),
    staff_views integer,
    mins_to_first_assign integer,
    rating_1 character varying(20),
    rating_2 character varying(20)
);


--
-- Name: user_info; Type: TABLE; Schema: base; Owner: -
--

CREATE TABLE base.user_info (
    id character varying(50) NOT NULL,
    created character varying(30) NOT NULL,
    gender character(2),
    age_bin character(10),
    removed character(2),
    listed character(2),
    listed_since character varying(30),
    city character varying(30),
    fax character varying,
    email character varying
);


--
-- Name: view_conversation; Type: VIEW; Schema: analytics; Owner: -
--

CREATE VIEW analytics.view_conversation AS
 SELECT (user_info.created)::timestamp without time zone AS user_created,
    (user_info.gender)::boolean AS gender,
    user_info.age_bin,
    (user_info.removed)::boolean AS removed,
    (user_info.listed)::boolean AS listed,
    user_info.listed_since,
    user_info.city,
    conversation_info.id,
    conversation_info.patient_id,
    (conversation_info.conversation_opened)::timestamp without time zone AS conversation_opened,
    (conversation_info.conversation_closed)::timestamp without time zone AS conversation_closed,
    (conversation_info.created_by_staff)::boolean AS created_by_staff,
    conversation_info.first_assigned,
    conversation_info.category,
    conversation_info.staff_1_role,
    conversation_info.staff_2_role,
    conversation_info.staff_3_role,
    conversation_info.staff_4_role,
    (conversation_info.call_in_conversation)::boolean AS call_in_conversation,
    conversation_info.call_time_in_seconds,
    conversation_info.sip_call_time_in_seconds,
    conversation_info.number_of_messages,
    conversation_info.number_of_bot_messages,
    conversation_info.number_of_client_messages,
    conversation_info.number_of_staff_messages,
    (conversation_info.patient_cancelled)::boolean AS patient_cancelled,
    conversation_info.highest_level_of_care,
    conversation_info.staff_views,
    conversation_info.mins_to_first_assign,
    conversation_info.rating_1
   FROM (base.conversation conversation_info
     LEFT JOIN base.user_info user_info ON (((conversation_info.patient_id)::text = (user_info.id)::text)));


--
-- Name: queue_process; Type: TABLE; Schema: base; Owner: -
--

CREATE TABLE base.queue_process (
    id bigint NOT NULL,
    conversation character varying(50) NOT NULL,
    queue_name character varying(20),
    queue_start character varying(30),
    queue_end character varying(30),
    category character varying(30),
    mins_to_assign character varying(30),
    estimated_mins character varying(30)
);


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying(255) NOT NULL
);


--
-- Name: conversation conversation_pkey; Type: CONSTRAINT; Schema: base; Owner: -
--

ALTER TABLE ONLY base.conversation
    ADD CONSTRAINT conversation_pkey PRIMARY KEY (id);


--
-- Name: queue_process queue_process_pkey; Type: CONSTRAINT; Schema: base; Owner: -
--

ALTER TABLE ONLY base.queue_process
    ADD CONSTRAINT queue_process_pkey PRIMARY KEY (id, conversation);


--
-- Name: user_info user_info_pkey; Type: CONSTRAINT; Schema: base; Owner: -
--

ALTER TABLE ONLY base.user_info
    ADD CONSTRAINT user_info_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- PostgreSQL database dump complete
--


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20201012131510'),
    ('20201012131739'),
    ('20201012133714'),
    ('20201012140807'),
    ('20201012191415'),
    ('20201012192448');
