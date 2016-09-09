# inugami

An Erlang/OTP library for generating universally unique identifiers (UUID), as defined in [RFC 4122](https://tools.ietf.org/html/rfc4122). That includes versions 1, 3, 4, and 5 of the specification.

## Requirements

* [Erlang/OTP](http://www.erlang.org) R17 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

## Building and Testing

```
$ rebar3 compile
$ rebar3 ct
```

## TODO

* Rename several functions and change some types
    - probably everything `bitstring` can just be `binary`
    - `bitstring_to_bin/1` => `hex_bitstring_to_binary/1`
    - `bin_to_bitstring/1` => `binary_to_hex_bitstring/1`
    - `uuid/6` => `new_uuid/6`
* Commit and tag as 1.0.0

## License

[BSD 3-Clause](https://opensource.org/licenses/BSD-3-Clause), see the `LICENSE` file.
