# Policyj

This is an implementation of a policyd for the Postfix MTA.
It implements blocklists via DNS and Databases.

# Status

We're currently rearranging the code so that it's usable outside its current
setting. We've developed the daemon for our company but are opensourcing it now.
The monitoring handler is currently unflexible and taylored to a specific output
which had to be maintained but can be generalized and cleaned up now.

# Features

* DNSBL
* Database based blocklists
* Logging in key-value format that's easily parsable by e.g. logstash


## Getting started

Coming soon

## Limitations

Currently the db blocklists only support MySQL.
This is rather unfortunate but a result of constraints during
the development. You can add postgresql support relatively simple if
you like.

## Running tests

### Configure the database
make sure that your database is configured to work with the
credentials given in test/helpers/db

### Lein test

```
$ lein test
```
