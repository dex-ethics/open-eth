BEGIN;

--------------------------------------------------------------------------------
-- We use JSON Web Tokens to authenticate API requests. PostgREST
-- cares specifically about a claim called role. When request
-- contains a valid JWT with a role claim PostgREST will switch
-- to the database role with that name for the duration of the
-- HTTP request. If the client included no (or an invalid) JWT
-- then PostgREST selects the role "anonymous".

CREATE USER authenticator WITH NOINHERIT LOGIN
	PASSWORD 'vGsg4-Nxkn0xpSFRCthXDN';
CREATE ROLE anonymous;
CREATE ROLE author;
GRANT anonymous, author TO authenticator;

GRANT USAGE ON SCHEMA public TO anonymous, author;

--------------------------------------------------------------------------------
-- The user id is a string stored in postgrest.claims.sub. Let's
-- wrap this in a nice function.

CREATE or replace FUNCTION current_user_id() RETURNS text
STABLE
LANGUAGE plpgsql
AS $$
BEGIN
	RETURN current_setting('postgrest.claims.userid');
EXCEPTION
	-- handle unrecognized configuration parameter error
	WHEN undefined_object THEN RETURN '';
END;
$$;

GRANT EXECUTE ON FUNCTION current_user_id()
	TO anonymous, author;

--------------------------------------------------------------------------------

CREATE TABLE users (
	id            text      primary key,
	created       timestamp not null default now(),
	last_login    timestamp not null default now(),
	name          text      not null,
	nickname      text      ,
	avatar        text      ,
	profile       json
);

-- TODO: Users add their own profiles
-- TODO: Users update their own profiles
-- TODO: Users can not enumerate profiles
-- TODO: Users can select all except the 'profile' field

--------------------------------------------------------------------------------

COMMIT;
