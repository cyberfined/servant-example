-- migrate:up
CREATE TABLE users(
    id SERIAL PRIMARY KEY,
    email text UNIQUE NOT NULL,
    login text UNIQUE NOT NULL,
    password_hash bytea NOT NULL,
    image text,
    bio text,
    created_at TIMESTAMP NOT NULL default now()
);

-- migrate:down
DROP TABLE users CASCADE;
