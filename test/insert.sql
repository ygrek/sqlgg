create table test ( foo integer not null default 12345 );
insert into test ( foo ) values ( default );
select * from test where foo != default(foo);
insert into test VALUES @x;
insert into test(foo) VALUES @x;
create table test2 ( foo integer not null default 12345, bar text not null default '');
insert into test2 VALUES @x;
insert into test2(foo, bar) VALUES @x ON DUPLICATE KEY UPDATE bar = @on_collision;
