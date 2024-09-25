CREATE TABLE IF NOT EXISTS account_types ( 
    type_id INT NOT NULL PRIMARY KEY, 
    type_name VARCHAR(255) NOT NULL 
);
CREATE TABLE IF NOT EXISTS users (
    id INT NOT NULL, 
    user_id INT NOT NULL PRIMARY KEY, 
    name VARCHAR(255), 
    email VARCHAR(255), 
    account_type_id INT NULL, 
    FOREIGN KEY (account_type_id) 
    REFERENCES account_types(type_id)
);

SELECT 
    users.name,
    users.email, 
    account_types.type_name FROM users 
LEFT JOIN account_types ON users.account_type_id = account_types.type_id;
