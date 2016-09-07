# inugami

An Erlang/OTP library for generating universally unique identifiers (UUID), as defined in [RFC 4122](https://tools.ietf.org/html/rfc4122). At the moment, only version 4 is generated.

## Requirements

* [Erlang/OTP](http://www.erlang.org) R17 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

## Building and Testing

```
$ rebar3 compile
$ rebar3 ct
```

## TODO

* Add richer tests for UUID version 1
* Document with examples
