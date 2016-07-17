create table profiles (id int, host text, today_count int);
select id, host, if(@x < now() - interval 1 day, 0, today_count) from profiles;
