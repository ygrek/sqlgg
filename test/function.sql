CREATE OR REPLACE FUNCTION ns.increment(i INT) RETURNS INT AS $$
BEGIN
    RETURN i + 1;
END;
$$ LANGUAGE plpgsql;

SELECT ns.increment(2);
