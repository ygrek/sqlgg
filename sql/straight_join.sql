-- @create_table_a
CREATE TABLE `a` (
    `id` INT AUTO_INCREMENT PRIMARY KEY,
    `b_id` INT,
    `good_condition` BOOLEAN
);

-- @create_table_b
CREATE TABLE `b` (
    `id` INT AUTO_INCREMENT PRIMARY KEY
);

-- @get_all_a_id_related_to_b
SELECT a.`id`
FROM a STRAIGHT_JOIN b ON
  a.`b_id` = b.`id`
WHERE a.`b_id` mod @some_a = @some_b AND `good_condition`;
