CREATE TABLE IF NOT EXISTS `master` (
  `id` INTEGER PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS `detail` (
  `id` INTEGER PRIMARY KEY,
  `master_id` INTEGER,
  FOREIGN KEY (`master_id`) REFERENCES `master` (`id`)
);

SELECT m.`id` m_id, d.`id` d_id
FROM `master` m
LEFT JOIN `detail` d ON d.`id` = (
  SELECT dd.`id`
  FROM `detail` dd
  WHERE dd.`master_id` = m.`id`
  ORDER BY dd.`id` DESC
  LIMIT 1
);

SELECT x.* FROM (
  SELECT 1, 'foo', NULL
) x;

SELECT x.* FROM (
  SELECT 1, 2 UNION
  SELECT 3, 4
) x;

SELECT x.* FROM (
  SELECT 'foo' UNION ALL
  SELECT 'foo'
) x;
