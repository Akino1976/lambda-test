-- migrate:up
CREATE OR REPLACE FUNCTION base.increment(i integer) 
RETURNS integer AS 
$_$
    BEGIN
            RETURN i + 1;
    END;
$_$ LANGUAGE plpgsql;

-- migrate:down

DROP FUNCTION IF EXISTS base.increment;