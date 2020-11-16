

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;


SET SESSION AUTHORIZATION DEFAULT;

ALTER TABLE public.posts DISABLE TRIGGER ALL;

INSERT INTO public.posts (id, title, body, created_at) VALUES ('f1b80d38-035e-4ae4-8718-f119d4475448', 'First Post', 'Hello world', '2020-11-16 10:43:59.03125+08');
INSERT INTO public.posts (id, title, body, created_at) VALUES ('851edbdd-9289-4f99-957b-48c23492c4a1', 'Second Post', '|> validateField #body n', '2020-11-16 10:43:59.031873+08');


ALTER TABLE public.posts ENABLE TRIGGER ALL;


ALTER TABLE public.comments DISABLE TRIGGER ALL;

INSERT INTO public.comments (id, author, body, post_id) VALUES ('1bd6aed6-0bd8-46ee-a4c4-1012118d26fa', 'rc', 'testa', '851edbdd-9289-4f99-957b-48c23492c4a1');
INSERT INTO public.comments (id, author, body, post_id) VALUES ('24d31837-4365-4db5-afaf-38b9335d540a', 'www', 'test', '851edbdd-9289-4f99-957b-48c23492c4a1');


ALTER TABLE public.comments ENABLE TRIGGER ALL;


