CREATE TABLE test_status (
  status ENUM('Failed','Skipped','Passed') NOT NULL
);

-- @test1
SELECT * FROM test_status WHERE status IN @status_list;

CREATE TABLE users (
  id INT NOT NULL,
  status ENUM('Active','Inactive','Pending') NOT NULL
);

-- @test2
INSERT INTO users (id, status) VALUES @rows;

