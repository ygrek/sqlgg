create table test ( foo integer not null default 12345 );
insert into test ( foo ) values ( default );
select * from test where foo != default(foo);
