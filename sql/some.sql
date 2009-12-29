create table [OrderLine] (Id int, OrderId int, ProductId int, Qty int, Status int);
create table [Product] (Id int, Name text, Price float);

select OrderId, count(*) 
from OrderLine
group by OrderId
having count(*) > @cnt;

-- update %s;

select sum(p.Price * ol.Qty) as Price
from OrderLine ol
inner join Product p on ol.ProductId = p.Id
where ol.OrderId = @id;

