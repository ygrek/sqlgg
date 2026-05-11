CREATE TABLE users (id INT NOT NULL, email VARCHAR(255), UNIQUE KEY uq_a (email), UNIQUE KEY uq_b (email));
