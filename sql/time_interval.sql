
CREATE TABLE elem (
    id INTEGER,
    name VARCHAR,
    num INTEGER
);
CREATE TABLE proxy (
    elem_id INTEGER,
    time_id INTEGER
);
CREATE TABLE time (
    id INTEGER,
    timestamp DATESTAMP
);

INSERT INTO elem VALUES (0,"one",1);
INSERT INTO elem VALUES (1,"one",2);
INSERT INTO elem VALUES (2,"one",3);
INSERT INTO elem VALUES (3,"two",1);

INSERT INTO proxy VALUES (0,0);
INSERT INTO proxy VALUES (0,1);
INSERT INTO proxy VALUES (1,2);
INSERT INTO proxy VALUES (1,3);
INSERT INTO proxy VALUES (2,4);
INSERT INTO proxy VALUES (3,4);

INSERT INTO time VALUES (0,"12/12/2008");
INSERT INTO time VALUES (1,"13/12/2008");
INSERT INTO time VALUES (2,"14/12/2008");
INSERT INTO time VALUES (3,"15/12/2008");
INSERT INTO time VALUES (4,"16/12/2008");

CREATE temp VIEW all_t AS SELECT elem.name,elem.num,time.timestamp FROM elem,time,proxy WHERE proxy.elem_id = elem.id AND time.id = proxy.time_id ;

SELECT name,num,min(timestamp),max(timestamp) FROM all_t GROUP BY name, num;

