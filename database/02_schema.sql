begin;

create table dilemma (
	id bigserial primary key,
	created date not null,
	name text not null,
	description text not null
);

commit;
