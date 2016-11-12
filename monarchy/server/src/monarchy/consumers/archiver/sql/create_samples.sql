-- there is currently no jdbc json type so we must create an implicit cast here
create cast (text as json) without function as implicit;
create cast (varchar as json) without function as implicit;

CREATE TABLE samples (
   id bigserial PRIMARY KEY,
   sender_uuid varchar(36) NOT NULL CHECK (length(sender_uuid) = 36),
   sent_at     timestamp NOT NULL,
   received_at timestamp NOT NULL DEFAULT NOW(),
   body json NOT NULL
)
