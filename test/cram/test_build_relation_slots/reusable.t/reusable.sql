CREATE TABLE users (id INT NOT NULL, status TEXT NOT NULL);
CREATE TABLE archived (id INT NOT NULL, status TEXT NOT NULL);

-- @active_users | include: reuse
SELECT id, status FROM users WHERE status = @status;

-- @recent_users | include: reuse
SELECT id, status FROM users WHERE id > @min_id AND status <> @excluded;

-- @named_table | include: reuse
-- [sqlgg] subst=t
SELECT id, status FROM `%%t%%`;

-- @by_choice | include: reuse
SELECT id, status FROM users WHERE @c { All { TRUE } | Only { status = @st } };

-- @pick
SELECT @before AS before, s.id FROM @src AS s WHERE s.id > @after;
