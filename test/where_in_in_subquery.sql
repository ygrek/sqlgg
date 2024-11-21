CREATE TABLE test30 (
    id INT PRIMARY KEY, 
    column_a TEXT, 
    column_b BOOL, 
    column_c_31 INT NOT NULL
);

CREATE TABLE test31 (
    id INT, 
    column_d INT, 
    column_c_32 INT
);

CREATE TABLE test32 (
    id INT, 
    column_f INT
);

UPDATE test30 t30
SET
    t30.column_a = 'the value',
    t30.column_b = false
WHERE
    t30.column_c_31 IN (
        SELECT t31.id
        FROM test31 t31
        WHERE t31.column_c_32 IN (
            SELECT t32.id
            FROM test32 t32
            WHERE t32.column_f IN @c_f_ids
        )
    );

CREATE TABLE tbl1 (
    id INT AUTO_INCREMENT PRIMARY KEY,
    col1 INT,
    col2 VARCHAR(50)
);

CREATE TABLE tbl2 (
    id INT AUTO_INCREMENT PRIMARY KEY,
    col1 INT,
    col2 INT,
    tbl1_id INT
);

CREATE TABLE tbl3 (
    id INT AUTO_INCREMENT PRIMARY KEY,
    col1 INT,
    col2 INT,
    col3 VARCHAR(100),
    tbl2_id INT
);

CREATE TABLE tbl4 (
    id INT AUTO_INCREMENT PRIMARY KEY,
    col1 INT,
    col2 INT,
    col3 DECIMAL(10,2),
    tbl3_id INT
);

UPDATE tbl4 t4
SET t4.col3 = t4.col3 * 1.2
WHERE t4.tbl3_id IN (
    SELECT t3.id
    FROM tbl3 t3
    WHERE t3.col2 IN (
        SELECT t2.col2
        FROM tbl2 t2
        WHERE t2.col1 IN (
            SELECT t1.id
            FROM tbl1 t1
            WHERE t1.col2 IN @t1_c2
        )
    )
);

UPDATE tbl3 t3
SET t3.col3 = CONCAT('Updated: ', t3.col3)
WHERE t3.tbl2_id IN (
    SELECT t2.id
    FROM tbl2 t2
    WHERE t2.tbl1_id IN (
        SELECT t1.id
        FROM tbl1 t1
        WHERE t1.col1 IN  @t1_c1
    )
);

UPDATE tbl2 t2
SET t2.col1 = t2.col1 + 1000
WHERE t2.id IN (
    SELECT t3.tbl2_id
    FROM tbl3 t3
    WHERE t3.col2 IN (
        SELECT t4.col2
        FROM tbl4 t4
        WHERE t4.col1 IN (
            SELECT t1.id
            FROM tbl1 t1
            WHERE t1.col2 IN  @t1_c2
        )
    )
);

UPDATE tbl2 t2
SET t2.col1 = t2.col1 + 1000
WHERE t2.id IN (
    SELECT t3.tbl2_id
    FROM tbl3 t3
    WHERE t3.col2 IN (
        SELECT t4.col2
        FROM tbl4 t4
        WHERE @left_side_isnt_missed IN (
            SELECT t1.id
            FROM tbl1 t1
            WHERE t1.col2 IN  @t1_c2
        )
    )
);
