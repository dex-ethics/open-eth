BEGIN;

-- We use JSON Web Tokens to authenticate API requests. PostgREST
-- cares specifically about a claim called role. When request
-- contains a valid JWT with a role claim PostgREST will switch
-- to the database role with that name for the duration of the
-- HTTP request. If the client included no (or an invalid) JWT
-- then PostgREST selects the role "anonymous".

CREATE USER authenticator WITH NOINHERIT LOGIN
	PASSWORD 'vGsg4-Nxkn0xpSFRCthXDN';
CREATE ROLE anonymous;
CREATE ROLE admin;
CREATE ROLE author;
GRANT author, admin, anonymous TO authenticator;

GRANT USAGE ON SCHEMA public TO anonymous, author;

-- The user id is a string stored in postgrest.claims.sub. Let's
-- wrap this in a nice function.

CREATE FUNCTION current_user_id()
RETURNS text
STABLE
LANGUAGE SQL
AS $$
	SELECT current_setting('postgrest.claims.sub');
$$;

COMMIT;
