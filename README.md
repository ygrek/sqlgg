sqlgg: SQL Guided (code) Generator
==================================

[![Build Status](https://travis-ci.org/ygrek/sqlgg.svg?branch=master)](https://travis-ci.org/ygrek/sqlgg)
[![Build status](https://ci.appveyor.com/api/projects/status/1bfx41oulkbu4aj2?svg=true)](https://ci.appveyor.com/project/ygrek/sqlgg/branch/master)

Homepage at http://ygrek.org.ua/p/sqlgg/

Get the code at http://repo.or.cz/w/sqlgg.git or https://github.com/ygrek/sqlgg

Dependencies
------------

Install with OPAM: `opam install menhir extlib ounit ppx_deriving oasis`

Build
-----

Run `make` (or `build.bat` on Windows).

Windows users
-------------

Install [VS2005 SP1 redistributable](http://www.microsoft.com/downloads/details.aspx?FamilyID=200b2fd9-ae1a-4a14-984d-389c36f85647)

Conditions
----------

```
Copyright (c) 2009 ygrek <ygrek@autistici.org>

This project is distributed under the terms of GPL Version 2. See LICENSE file for full license text.

Example code in demo/ and example/ and database specific bindings in impl/ are released into public domain.
See UNLICENSE file in each of the above directories for more information.
```

NB the output of sqlgg, i.e. the generated code, is all yours of course :)

----
2018-03-10
