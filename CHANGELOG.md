# Version 1.1.11

* Update copyright year to 2021
* recv_byte returns {ok, _} or throws an error, but never returns {error, _}

# Version 1.1.10

* Fix Coveralls command call
* Fix Travis setup using Rebar3

# Version 1.1.9

* Update copyright to 2020

# Version 1.1.8

* Update for hex.pm release

# Version 1.1.7

* Add contribution guide

# Version 1.1.6

* Add support for ipv6 connections

# Version 1.1.5

* Fix compilation with rebar3

# Version 1.1.4

* Update coverall script

# Version 1.1.3

* Make it possible to set connect timeout

# Version 1.1.2

* Add SSL support (Evgeniy Khramtsov)

# Version 1.1.1

* adding timeout to pgsql:squery (Felipe Ripoll)
* Decode int4 and int8 as signed integers (Alexey Shchepin)
* Better error handling in pgsql:squery (Alexey Shchepin)

# Version 1.1.0

* Fixes 'prepare' and 'execute' calls (Alexey Shchepin)
* Refactor (Alexey Shchepin)

# Version 1.0.1

* Repository is now called p1_pgsql for consistency (Mickaël Rémond)
* Initial release on Hex.pm (Mickaël Rémond)
* Standard ProcessOne build chain (Mickaël Rémond)
* Setup Travis-CI and test coverage, tests still needed (Mickaël Rémond)
