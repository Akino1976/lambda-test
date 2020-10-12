-- migrate:up
ALTER TABLE base.user_info 
    ADD COLUMN fax VARCHAR,
    ADD COLUMN email VARCHAR;

-- migrate:down

ALTER TABLE base.user_info 
    DROP COLUMN fax, 
    DROP COLUMN email;
