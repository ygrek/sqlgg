CREATE TABLE IF NOT EXISTS random_table_1 (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `id2` int(10) unsigned NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

SELECT id FROM random_table_1
WHERE 
  @id {
    `Another` { (`id`, `id2`) IN @ids } |
    `AndAnother` { @x > 2 } |
    `Lol` { `id` IN @idss }
  } 
  OR { (`id`, `id2`) IN @ids2 }?;  

SELECT id FROM random_table_1
WHERE (`id`, `id2`) IN @ids2;
