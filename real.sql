 create table codeTable (locat INT, code TEXT, descript TEXT);

 select * 
 from codeTable
 where locat not in (30, 50)
 and code not in
   (
    select code
    from 
      (select code, descript     // gets unique code-and-descript combos
       from codeTable
       where locat not in (30, 50)
       group by code, descript
      )
    group by code
    having count(*) > 1     // (error? should be "="?)
   )
 order by code, locat;
