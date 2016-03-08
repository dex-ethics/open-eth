BEGIN;

CREATE TABLE dilemma (
	id            bigserial primary key,
	created       date not null,
	name          text not null,
	description   text not null,
	author        text not null default current_user_id()
);

GRANT SELECT ON TABLE dilemma TO anonymous;
GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE dilemma TO author;
GRANT USAGE, SELECT ON SEQUENCE dilemma_id_seq TO author;
ALTER TABLE dilemma ENABLE ROW LEVEL SECURITY;
CREATE POLICY author_eigenedit ON dilemma
	USING (TRUE)
	WITH CHECK (
		author = current_user_id()
	);

COMMIT;
