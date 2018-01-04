
    buck=# create database gem;
    CREATE DATABASE
    buck=# \c gem
    psql (9.6.2, server 9.5.1)
    You are now connected to database "gem" as user "buck".
    gem=# create schema gem;
    CREATE SCHEMA
    gem=# create table gem.notepads (
    id serial primary key,
    description text,
    title character varying(100), creator_token character varying(50), contents jsonb, created_at timestamptz, updated_at timestamptz
    );
    CREATE TABLE
    gem=# INSERT INTO gem.notepads (title, creator_token, description) VALUES ('My cool notepad', 'a76asd', 'this is a cool notepad');
    INSERT 0 1
