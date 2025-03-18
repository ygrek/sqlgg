CREATE TABLE tbl_1 (
    col_1_1 INT AUTO_INCREMENT PRIMARY KEY,
    col_1_2 VARCHAR(100) NOT NULL,
    col_1_3 VARCHAR(50) NOT NULL,
    col_1_4 FLOAT DEFAULT 0.0,
    col_1_5 BOOLEAN DEFAULT FALSE
) ENGINE=InnoDB;

CREATE TABLE tbl_2 (
    col_2_1 INT AUTO_INCREMENT PRIMARY KEY,
    col_2_2 INT NOT NULL,
    col_2_3 DATE NOT NULL,
    col_2_4 VARCHAR(20) NOT NULL
) ENGINE=InnoDB;

CREATE TABLE tbl_3 (
    col_3_1 INT AUTO_INCREMENT PRIMARY KEY,
    col_3_2 INT NOT NULL,
    col_3_3 INT NOT NULL,
    col_3_4 INT DEFAULT 1,
    col_3_5 FLOAT DEFAULT 0.0,
    FOREIGN KEY (col_3_2) REFERENCES tbl_2(col_2_1) ON DELETE CASCADE,
    FOREIGN KEY (col_3_3) REFERENCES tbl_1(col_1_1) ON DELETE CASCADE
) ENGINE=InnoDB;

SELECT col_1_2, col_1_3, col_1_4 
FROM tbl_1
WHERE col_1_1 IN (
    SELECT col_1_1 
    FROM tbl_1
    WHERE @var_1 { 
        Pat_1 { col_1_3 = 'cat_a' } 
        | Pat_2 { col_1_4 > 300.00 }
    }
);

SELECT t2.col_2_1, t2.col_2_4
FROM tbl_2 t2
WHERE col_2_1 IN (
    SELECT col_2_1 
    FROM tbl_2
    WHERE @var_2 {
        Pat_3 { t2.col_2_4 = 'stat_1' }
        | Pat_4 {
            EXISTS (
                SELECT 1 
                FROM tbl_3 t3
                JOIN tbl_1 t1 ON t3.col_3_3 = t1.col_1_1
                WHERE t3.col_3_2 = t2.col_2_1
                AND t1.col_1_3 = 'cat_a'
            )
        }
    }
);

SELECT t1.col_1_2, t1.col_1_3, t1.col_1_4
FROM tbl_1 t1
WHERE t1.col_1_1 IN (
    SELECT col_1_1
    FROM tbl_1
    WHERE @var_4 {
        Pat_7 { 
            col_1_3 = 'cat_a' AND
            @var_5 {
                SubPat_1 { col_1_4 > 200 }
                | SubPat_2 { col_1_4 <= 200 }
            }
        }
        | Pat_8 {
            col_1_3 = 'cat_b' AND
            @var_6 {
                SubPat_3 { col_1_5 = TRUE }
                | SubPat_4 { col_1_5 = FALSE }
            }
        }
    }
);

SELECT t2.col_2_1, t2.col_2_2, t2.col_2_4
FROM tbl_2 t2
WHERE t2.col_2_1 IN (
    SELECT t2.col_2_1
    FROM tbl_2
    WHERE @var_7 {
        Pat_9 {
            t2.col_2_4 = 'stat_1' AND
            EXISTS (
                SELECT 1
                FROM tbl_3 t3
                WHERE t3.col_3_2 = t2.col_2_1
                GROUP BY t3.col_3_2
                HAVING COUNT(*) > 1
            )
        }
        | Pat_10 {
            t2.col_2_4 = 'stat_3' AND
            EXISTS (
                SELECT 1
                FROM tbl_3 t3
                WHERE t3.col_3_2 = t2.col_2_1 AND
                t3.col_3_5 * t3.col_3_4 > 300
            )
        }
        | Pat_11 {
            @var_8 {
                SubPat_5 { 
                    t2.col_2_2 IN (
                        SELECT col_2_2
                        FROM tbl_2
                        GROUP BY col_2_2
                        HAVING COUNT(*) > 1
                    )
                }
                | SubPat_6 {
                    t2.col_2_3 > '2024-03-01'
                }
            }
        }
    }
);
