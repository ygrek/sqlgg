CREATE TABLE products (
    id SERIAL PRIMARY KEY,       
    name VARCHAR(255) NOT NULL
);

CREATE TABLE entitlements (
    id SERIAL PRIMARY KEY,      
    product_id INT NOT NULL,     
    entitlement INT NOT NULL
);

SELECT entitlement, product_name
FROM products p
JOIN ( VALUES @values :: (Text, Int) ) AS x (product_name, entitlement)
ON p.name = x.product_name;

INSERT INTO entitlements ( product_id, entitlement )
SELECT p.id, x.entitlement
FROM products p
JOIN ( VALUES ROW('a', 1), ROW('b', 2), ROW('c', 3) ) AS x (product_name, entitlement)
ON p.name = x.product_name;
