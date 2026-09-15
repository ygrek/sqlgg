CREATE TABLE subst_items (
  id INT NOT NULL,
  score INT NOT NULL
);

-- @dynamic_projection
SELECT @f {
  A { id IN @ids AND score = @n }
  | B { TRUE }
} AS selected
FROM subst_items;

CREATE TABLE users (
  id INT NOT NULL PRIMARY KEY
);

CREATE TABLE profiles (
  user_id INT NOT NULL PRIMARY KEY,
  bio TEXT
);

-- @dynamic_join_with_choice
SELECT users.id, profiles.bio
FROM users
LEFT JOIN profiles ON profiles.user_id = users.id
WHERE @filter {
  A { users.id IN @filter_ids AND users.id = @filter_n }
  | B { TRUE }
};
