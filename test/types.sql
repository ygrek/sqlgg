-- issue #41
-- [sqlgg] subst=tablename
-- [sqlgg] name=create_table_files
CREATE TABLE `%%tablename%%` (
  first_key varbinary(30000) NOT NULL DEFAULT ''
) ENGINE=INNODB;

-- @by_prefix
-- [sqlgg] subst=tablename
select *
  from `%%tablename%%`
 where 'k' < first_key or first_key like concat(@k,'%');
