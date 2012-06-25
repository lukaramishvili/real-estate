-- Table: tr

-- DROP TABLE tr;

CREATE TABLE tr
(
  "ix-tr" integer NOT NULL,
  keyword text NOT NULL,
  lang text NOT NULL,
  "value" text NOT NULL,
  CONSTRAINT pri_ix_tr PRIMARY KEY ("ix-tr")
)
WITH (
  OIDS=FALSE
);
ALTER TABLE tr OWNER TO re_user;
