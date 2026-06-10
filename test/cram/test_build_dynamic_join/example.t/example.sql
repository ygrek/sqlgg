CREATE TABLE users (
  id INT PRIMARY KEY,
  org_id INT NOT NULL,
  name TEXT NOT NULL,
  email TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL,
  deleted BOOLEAN NOT NULL
);
CREATE TABLE profiles (
  user_id INT PRIMARY KEY,
  bio TEXT,
  avatar_url TEXT,
  location TEXT,
  website TEXT
);
CREATE TABLE billing (
  user_id INT PRIMARY KEY,
  plan TEXT NOT NULL,
  paid_until DATETIME,
  balance INT NOT NULL
);
-- [sqlgg] dynamic_select=true
-- @user_info
SELECT u.id, u.name, u.email, u.created_at,
       p.bio, p.avatar_url, p.location, p.website,
       b.plan, b.paid_until, b.balance
FROM users u
LEFT JOIN profiles p ON p.user_id = u.id
LEFT JOIN billing  b ON b.user_id = u.id
WHERE u.org_id = @org AND u.deleted = FALSE;
