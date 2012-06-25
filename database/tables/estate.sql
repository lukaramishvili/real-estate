-- Table: estate

-- DROP TABLE estate;

CREATE TABLE estate
(
  "ix-estate" integer NOT NULL,
  address text NOT NULL,
  telnum text NOT NULL,
  CONSTRAINT pri_key_estate PRIMARY KEY ("ix-estate")
)
WITH (
  OIDS=FALSE
);
ALTER TABLE estate OWNER TO re_user;
