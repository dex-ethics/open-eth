BEGIN;

CREATE TABLE dilemma (
	id            bigserial primary key,
	author        text      not null default current_user_id(),
	created       timestamp not null default now(),
	name          text      not null,
	description   text
);

-- Anonymous can read all
GRANT SELECT ON TABLE dilemma TO anonymous;

-- Author can modify name and description…
GRANT SELECT,
	INSERT (name, description),
	UPDATE (name, description),
	DELETE
ON TABLE dilemma TO author;
GRANT USAGE, SELECT ON SEQUENCE dilemma_id_seq TO author;

-- …but only of rows that he/she himself created.
ALTER TABLE dilemma ENABLE ROW LEVEL SECURITY;
CREATE POLICY author_eigenedit ON dilemma
	USING (TRUE)
	WITH CHECK (author = current_user_id());

COMMIT;
