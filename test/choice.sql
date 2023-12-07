CREATE TABLE test(k TEXT NULL);

INSERT INTO test (k) VALUES (@k { None { NULL } | Some { @k } });
