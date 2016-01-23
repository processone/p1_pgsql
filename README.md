# p1_pgsql

[![Build Status](https://travis-ci.org/processone/p1_pgsql.svg?branch=master)](https://travis-ci.org/processone/p1_pgsql) [![Coverage Status](https://coveralls.io/repos/processone/p1_pgsql/badge.svg?branch=master&service=github)](https://coveralls.io/github/processone/p1_pgsql?branch=master) [![Hex version](https://img.shields.io/hexpm/v/p1_pgsql.svg "Hex version")](https://hex.pm/packages/p1_pgsql)

Pure Erlang PostgreSQL driver

## Building

PostgreSQL driver can be build as follow:

    make

It is a rebar-compatible OTP application. Alternatively, you can build
it with rebar:

    rebar compile

## Development

### Test

#### Unit test

You can run eunit test with the command:

    $ rebar eunit
