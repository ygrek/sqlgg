CREATE TABLE test28 (
    employee_id INT,
    department_id INT,
    salary DECIMAL(10, 2)
);

CREATE TABLE test29 (
    department_id INT,
    department_name VARCHAR(100)
);

CREATE TABLE test30 (
    project_id INT,
    department_id INT,
    project_name VARCHAR(100)
);

CREATE TABLE test31 (
    employee_id INT,
    project_id INT
);

SELECT
    d.department_name,
    ds.total_salary,
    COALESCE(pc.project_count, 0) AS project_count,
    (ds.total_salary / NULLIF(COALESCE(pc.project_count, 0), 0)) AS avg_salary_per_project
FROM
    (
        SELECT
            e.department_id,
            SUM(e.salary) AS total_salary
        FROM
            test28 e
        GROUP BY
            e.department_id
    ) AS ds
JOIN
    (
        SELECT
            p.department_id,
            COUNT(ep.project_id) AS project_count
        FROM
            test30 p
        LEFT JOIN
            test31 ep ON p.project_id = ep.project_id
        GROUP BY
            p.department_id
    ) AS pc ON ds.department_id = pc.department_id
JOIN
    test29 d ON ds.department_id = d.department_id
ORDER BY
    ds.total_salary DESC;
