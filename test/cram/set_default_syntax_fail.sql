CREATE TABLE IF NOT EXISTS random (
  `test` varchar(80) COLLATE utf8_bin NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

INSERT INTO `random`
SET
  `test` = { @test { A {'2'} | B {'2'} } }??;
