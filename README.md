lucene_parser [![Build Status](https://travis-ci.org/goncalotomas/lucene_parser.svg?branch=master)](https://travis-ci.org/goncalotomas/lucene_parser)
=====

lucene_parser is a project for parsing Lucene-like query syntax into tokens.

## Usage:

```erlang
  {ok, Query} = lucene_parser:parse("default_index", "default_field", "foo AND bar").
  Query == #intersection { ops=[#term { s="foo" }, #term { s="bar" }] }.
```

## Notes:
- All arguments are expected to be lists.
- The components of the query graph can be found in lucene_parser.hrl.
