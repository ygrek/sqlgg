create table transactions (amount float not null, `date` datetime);
-- @insert
insert into transactions values;
-- @select
select amount + 2.5 from transactions where `date` > @date and amount > @limit;
