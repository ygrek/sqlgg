-- http://metaclass.livejournal.com/381806.html?thread=3001454#t3001454

declare @log table (
	Id int identity(1,1) primary key,
	ShiftId int not null,
	State1 int not null,
	State2 int null
);

-- Completely random data, just as a sample
insert into @log (ShiftId, State1, State2)
select top 300 low as [ShiftId],
	cast(substring(cast(newid() as binary(16)), 1, 3) as int) as [State1], case
		when (number % 5) % 3 = 0 then null
		else cast(substring(cast(newid() as binary(16)), 1, 3) as int)
	end as [State2]
from master.dbo.spt_values where type = 'P'
order by newid();

-- Now get start and end conditions for each shift
select Id, ShiftId, State1, State2, case RN when 1 then 'Start' else 'End' end as [StateTime]
from (
	select *,
		row_number() over(partition by shiftid order by id) as [rn],
		count(*) over(partition by Shiftid) as [ShiftSize]
	from @log
	) sq
where rn in (1, ShiftSize)
order by ShiftId, rn;
