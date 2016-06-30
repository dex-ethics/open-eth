BEGIN;

--------------------------------------------------------------------------------

CREATE TABLE dilemmas (
	id            bigserial primary key,
	author        text      not null
	                        default current_user_id()
	                        references hidden.users (id),
	created       timestamp not null
	                        default now(),
	name          text      not null,
	description   text      ,
	actions       text[2]   ,
	features      text[]
);

-- TODO: constrain 'actions' to have two elements

-- Anonymous can read all.
GRANT SELECT ON TABLE dilemmas TO anonymous;

-- Author can modify name and description…
GRANT SELECT,
	INSERT (name, description, actions, features),
	UPDATE (name, description, actions, features),
	DELETE
ON TABLE dilemmas TO author;
GRANT USAGE, SELECT ON SEQUENCE dilemmas_id_seq TO author;

-- …but only of rows that he/she himself created.
ALTER TABLE dilemmas ENABLE ROW LEVEL SECURITY;

CREATE POLICY select_unsecure ON dilemmas FOR SELECT
	USING (TRUE);

CREATE POLICY author_eigencreate ON dilemmas FOR INSERT
	WITH CHECK (author = current_user_id());

CREATE POLICY author_eigenupdate ON dilemmas FOR UPDATE
	USING (author = current_user_id())
	WITH CHECK (author = current_user_id());

CREATE POLICY author_eigendelete ON dilemmas FOR DELETE
	USING (author = current_user_id());

--------------------------------------------------------------------------------

CREATE TABLE cases (
	id            bigserial   primary key,
	created       timestamp   not null
	                          default now(),
	dilemma       bigint      not null
	                          references dilemmas (id)
                             on delete cascade
                             on update cascade,
	position      integer     not null,
	name          text        not null,
	description   text        ,
	action        integer     ,
	features      integer[][]
);

-- TODO: Action constraint that it indexes in dilemma.actions
-- TODO: features constraint that its dimensions are
---      dilemma.actions.length × dilemma.features.length

-- Anonymous can read all.
GRANT SELECT ON TABLE cases TO anonymous;

-- Author of dilemma can modify…
GRANT SELECT,
	INSERT (dilemma, position, name, description, action, features),
	UPDATE (position, name, description, action, features),
	DELETE
	ON TABLE cases TO author;
GRANT USAGE, SELECT ON SEQUENCE cases_id_seq TO author;

-- …but only of cases belonging to dilemma's that he/she himself created.
ALTER TABLE cases ENABLE ROW LEVEL SECURITY;

CREATE POLICY select_unsecure ON cases FOR SELECT
	USING (TRUE);

CREATE POLICY author_eigencreate ON cases FOR INSERT
	WITH CHECK ((SELECT author FROM dilemmas WHERE dilemmas.id = dilemma) = current_user_id());

CREATE POLICY author_eigenupdate ON cases FOR UPDATE
	USING ((SELECT author FROM dilemmas WHERE dilemmas.id = dilemma) = current_user_id())
	WITH CHECK ((SELECT author FROM dilemmas WHERE dilemmas.id = dilemma) = current_user_id());

CREATE POLICY author_eigendelete ON cases FOR DELETE
	USING ((SELECT author FROM dilemmas WHERE dilemmas.id = dilemma) = current_user_id());


COMMIT;
