# inugami

An Erlang/OTP library for generating universally unique identifiers (UUID), as defined in [RFC 4122](https://tools.ietf.org/html/rfc4122). That includes versions 1, 3, 4, and 5 of the specification. Named after the [dog god](https://en.wikipedia.org/wiki/Inugami) of Japanese folklore, for no particular reason.

## Requirements

* [Erlang/OTP](http://www.erlang.org) R17 or higher
* [rebar3](https://github.com/erlang/rebar3/) 3.0.0 or higher

## Building and Testing

```
$ rebar3 compile
$ rebar3 ct
```

## Example Usage

Including as a dependency in your release, using rebar...

```
{deps, [
    {inugami, {git, "https://github.com/nlfiedler/inugami", {tag, "1.0.0"}}}
]}.
```

Generating a version 1 UUID and extracting properties from it...

```
> Uuid = inugami:uuid1().
{uuid,<<191,7,211,198>>,
      <<118,19>>,
      <<17,230>>,
      <<"£">>,<<"Ã">>,
      <<0,62,225,194,211,88>>}
> inugami:get_version(Uuid).
1
> inugami:get_variant(Uuid).
variant_rfc4122
> inugami:get_node(Uuid).
<<"00:3e:e1:c2:d3:58">>
> inugami:get_timestamp(Uuid).
{1473,373786,279623}
> inugami:encode(Uuid).
<<"bf07d3c6-7613-11e6-a3c3-003ee1c2d358">>
> inugami:to_string(compact, Uuid).
"bf07d3c6761311e6a3c3003ee1c2d358"
```

## License

[BSD 3-Clause](https://opensource.org/licenses/BSD-3-Clause), see the `LICENSE` file.
