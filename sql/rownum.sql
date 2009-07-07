-- http://explainextended.com/2009/05/06/oracle-row_number-vs-rownum/
-- Oracle

CREATE TABLE mytable (
        id NUMBER(10) NOT NULL,
        paginator NUMBER(10) NOT NULL,
        `value` VARCHAR2(50)
);

ALTER TABLE mytable
ADD CONSTRAINT pk_mytable_id PRIMARY KEY (id);


CREATE INDEX ix_mytable_paginator_id ON mytable(paginator, id);


INSERT
INTO    mytable(id, paginator, `value`)
SELECT  level, level / 10000, 'Value ' || level
FROM    dual
CONNECT BY
        level <= 1000000;

SELECT  *
FROM    (
        SELECT  t.*, ROW_NUMBER() OVER (ORDER BY paginator, id) AS rn
        FROM    mytable t
        )
WHERE   rn BETWEEN 900001 AND 900010;

SELECT  *
FROM    (
        SELECT  t.*, ROWNUM AS rn
        FROM    mytable t
        ORDER BY
                paginator, id
        )
WHERE   rn BETWEEN 900001 AND 900010;

SELECT  *
FROM    (
        SELECT  t.*, ROWNUM AS rn
        FROM    mytable t
        ORDER BY
                paginator, id
        )
WHERE   rn >= 900001
        AND rownum <= 10;
