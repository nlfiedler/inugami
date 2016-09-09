%% -*- coding: utf-8 -*-
%%
%% Copyright 2016 Nathan Fiedler. All rights reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.
%%

%
% @author Nathan Fiedler <nathanfiedler@fastmail.fm>
% @copyright 2016 Nathan Fiedler
% @version 0.4.0
% @doc A library for decoding and generating UUIDs.
%
% `inugami' is an Erlang/OTP library for decoding and generating universally
% unique identifiers (UUID). It can be used to generate UUIDs of versions
% 1, 3, 4, and 5 of the RFC 4122 specification. In addition to generating
% UUIDs, `inugami' can decode strings and binaries into UUIDs, and extract
% certain values from the identifier, including the version, variant, node,
% and timestamp.
%
% Some example usage:
%
% ```
% > Uuid = inugami:uuid1().
% {uuid,<<191,7,211,198>>,
%       <<118,19>>,
%       <<17,230>>,
%       <<"£">>,<<"Ã">>,
%       <<0,62,225,194,211,88>>}
% > inugami:get_version(Uuid).
% 1
% > inugami:get_variant(Uuid).
% variant_rfc4122
% > inugami:get_node(Uuid).
% <<"00:3e:e1:c2:d3:58">>
% > inugami:get_timestamp(Uuid).
% {1473,373786,279623}
% > inugami:encode(Uuid).
% <<"bf07d3c6-7613-11e6-a3c3-003ee1c2d358">>
% > inugami:to_string(compact, Uuid).
% "bf07d3c6761311e6a3c3003ee1c2d358"
% '''
%
-module(inugami).

-export([nil/0, new_uuid/6, uuid1/0, uuid3/2, uuid4/0, uuid5/2]).
-export([decode/1, encode/1, urn/1, to_string/1, to_string/2]).
-export([get_version/1, set_version/2]).
-export([get_variant/1, set_variant/1]).
-export([get_node/1, get_timestamp/1]).
-export([namespace_dns/0, namespace_url/0, namespace_oid/0, namespace_x500/0]).
-export([hexbin_to_bin/1, bin_to_hexbin/1]).

% The difference in 100-nanosecond intervals between the UUID epoch
% (15 October 1582) and the Unix epoch (1 January 1970).
-define(NANOSECOND_INTERVALS_OFFSET, 122192928000000000).

-record(uuid, {
    time_low   :: binary,
    time_mid   :: binary,
    time_high  :: binary,
    clock_high :: binary,
    clock_low  :: binary,
    node       :: binary
}).
-type uuid() :: #uuid{
    time_low   :: binary,
    time_mid   :: binary,
    time_high  :: binary,
    clock_high :: binary,
    clock_low  :: binary,
    node       :: binary
}.
-export_type([uuid/0]).

% @doc
%
% Produce the UUID for the DNS (domain name system) namespace, as defined in
% section 4.3 of RFC 4122.
%
% @since 0.1.0
%
-spec namespace_dns() -> uuid().
namespace_dns() -> decode(<<"6ba7b810-9dad-11d1-80b4-00c04fd430c8">>).

% @doc
%
% Produce the UUID for the URL namespace, as defined in section 4.3 of RFC 4122.
%
% @since 0.1.0
%
-spec namespace_url() -> uuid().
namespace_url() -> decode(<<"6ba7b811-9dad-11d1-80b4-00c04fd430c8">>).

% @doc
%
% Produce the UUID for the OID (ISO Object IDs) namespace, as defined in section
% 4.3 of RFC 4122.
%
% @since 0.1.0
%
-spec namespace_oid() -> uuid().
namespace_oid() -> decode(<<"6ba7b812-9dad-11d1-80b4-00c04fd430c8">>).

% @doc
%
% Produce the UUID for the X.500 Distinquished Names namespace, as defined in
% section 4.3 of RFC 4122.
%
% @since 0.1.0
%
-spec namespace_x500() -> uuid().
namespace_x500() -> decode(<<"6ba7b814-9dad-11d1-80b4-00c04fd430c8">>).

% @doc
%
% Construct a UUID from the given parts, either binaries or integers.
%
% @since 0.2.0
%
-spec new_uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node) -> uuid()
      when TimeLow   :: binary() | integer(),
           TimeMid   :: binary() | integer(),
           TimeHigh  :: binary() | integer(),
           ClockHigh :: binary() | integer(),
           ClockLow  :: binary() | integer(),
           Node      :: binary() | integer().
new_uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node) when is_binary(TimeLow) ->
    #uuid{
        time_low = inugami:hexbin_to_bin(TimeLow),
        time_mid = inugami:hexbin_to_bin(TimeMid),
        time_high = inugami:hexbin_to_bin(TimeHigh),
        clock_high = inugami:hexbin_to_bin(ClockHigh),
        clock_low = inugami:hexbin_to_bin(ClockLow),
        node = inugami:hexbin_to_bin(Node)
    };
new_uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node) when is_integer(TimeLow) ->
    EncodeAndZeroPad = fun(Integer, Length) ->
        Subject = binary:encode_unsigned(Integer),
        Padding = binary:copy(<<0>>, Length - byte_size(Subject)),
        list_to_binary([Padding, Subject])
    end,
    #uuid{
        time_low = EncodeAndZeroPad(TimeLow, 4),
        time_mid = EncodeAndZeroPad(TimeMid, 2),
        time_high = EncodeAndZeroPad(TimeHigh, 2),
        clock_high = EncodeAndZeroPad(ClockHigh, 1),
        clock_low = EncodeAndZeroPad(ClockLow, 1),
        node = EncodeAndZeroPad(Node, 6)
    }.

% @doc
%
% Construct the nil UUID as described in section 4.1.7 of RFC 4122.
%
% @since 0.2.0
%
-spec nil() -> uuid().
nil() -> new_uuid(0, 0, 0, 0, 0, 0).

% @doc
%
% Generate a version 1 (time-based) universally unique identifier, as
% described in section 4.2.2 of RFC 4122.
%
% @since 0.2.0
%
-spec uuid1() -> uuid().
uuid1() ->
    <<TimeHigh:12, TimeMid:16, TimeLow:32>> = timestamp(),
    % No real clock sequence, just a random number, per section 4.1.5 of
    % RFC 4122.
    <<ClockHigh:6, ClockLow:8, _R/bits>> = crypto:strong_rand_bytes(2),
    % Make everything an integer so uuid/6 has an easy time.
    Node = binary:decode_unsigned(get_node()),
    Uuid = new_uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node),
    set_variant(set_version(Uuid, 1)).

% @doc
%
% Return a 60-bit timestamp value suitable for version 1 UUID. Uses the
% current system time and converts to the number of 100-nanosecond
% intervals since the UUID epoch (15 October 1582).
%
timestamp() ->
    % Convert the Unix epoch microseconds to nanoseconds (1 us = 1000 ns)
    % and divide that by 100 to get the number of 100-second intervals
    % since the UUID epoch. Or just multiply by 10 because math.
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    UnixEpochMicros = MegaSeconds * 1000000000000 + Seconds * 1000000 + MicroSeconds,
    UuidEpochNanos = ?NANOSECOND_INTERVALS_OFFSET + UnixEpochMicros * 10,
    % drops the 4 highest bits (where the UUID version goes)
    <<UuidEpochNanos:60>>.

% @doc
%
% Extract the time from a given UUID, in the same format as erlang:now/0, namely
% a tuple of {MegaSeconds, Seconds, MicroSeconds}.
%
% @since 0.3.0
%
-spec get_timestamp(uuid()) -> {integer(), integer(), integer()}.
get_timestamp(#uuid{}=Uuid) ->
    % clear the version bits (highest 4 bits) from the time_high field
    <<_:4, TH:12>> = Uuid#uuid.time_high,
    TimeHigh = <<0:4, TH:12>>,
    % reconstruct the original timestamp binary
    Tbin = list_to_binary([TimeHigh, Uuid#uuid.time_mid, Uuid#uuid.time_low]),
    % convert to an integer and do the math to get the time
    Tint = binary:decode_unsigned(Tbin),
    UnixEpochMicros = (Tint - ?NANOSECOND_INTERVALS_OFFSET) div 10,
    MegaSeconds = UnixEpochMicros div 1000000000000,
    MegaRemainder = UnixEpochMicros rem 1000000000000,
    Seconds = MegaRemainder div 1000000,
    MicroSeconds = MegaRemainder rem 1000000,
    {MegaSeconds, Seconds, MicroSeconds}.

% @doc
%
% Extract the node address from the given (version 1) UUID. Returned as a binary
% in colon-separated, hexadecimal format (e.g.
% &lt;&lt;"3c:07:54:7e:12:b0"&gt;&gt;).
%
% @since 0.3.0
%
-spec get_node(uuid()) -> binary().
get_node(#uuid{}=Uuid) ->
    ByteList = binary_to_list(Uuid#uuid.node),
    HexList = [lists:flatten(io_lib:format("~2.16.0b", [X])) || X <- ByteList],
    list_to_binary(string:join(HexList, ":")).

% @doc
%
% Find the network hardware address given the set of available interfaces.
% Avoid the loopback interface as its address is fixed. If no suitable
% address can be found, generate one with the multicast bit set to avoid
% conflict with addresses obtained from network cards. See section 4.1.6 of
% RFC 4122.
%
get_node() ->
    {ok, Interfaces} = inet:getifaddrs(),
    find_hwaddr(Interfaces).

% @doc
%
% Find a suitable network address, generating a random value if necessary.
%
find_hwaddr([{"lo", _IfConfig}|Rest]) ->
    find_hwaddr(Rest);
find_hwaddr([{_IfName, IfConfig}|Rest]) ->
    case lists:keyfind(hwaddr, 1, IfConfig) of
        {hwaddr, HwAddr} -> list_to_binary(HwAddr);
        false -> find_hwaddr(Rest)
    end;
find_hwaddr(_) ->
    <<NodeHigh:7, _:1, NodeLow:40>> = crypto:strong_rand_bytes(6),
    <<NodeHigh:7, 1:1, NodeLow:40>>.

% @doc
%
% Generate a version 3 (named-based MD5-hashed) universally unique
% identifier, as described in section 4.3 of RFC 4122.
%
% @since 0.2.0
%
-spec uuid3(uuid(), string() | binary()) -> uuid().
uuid3(#uuid{}=Namespace, Name) when is_list(Name); is_binary(Name) ->
    Digest = crypto:hash(md5, list_to_binary([
        Namespace#uuid.time_low,
        Namespace#uuid.time_mid,
        Namespace#uuid.time_high,
        Namespace#uuid.clock_high,
        Namespace#uuid.clock_low,
        Namespace#uuid.node,
        Name
    ])),
    <<TimeLow:32, TimeMid:16, TimeHigh:16, ClockHigh:8, ClockLow:8, Node:48>> = Digest,
    Uuid = new_uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node),
    set_variant(set_version(Uuid, 3)).

% @doc
%
% Generate a version 4 (random) universally unique identifier, as described
% in section 4.4 of RFC 4122.
%
% @since 0.1.0
%
-spec uuid4() -> uuid().
uuid4() ->
    Rand = crypto:strong_rand_bytes(16),
    <<TimeLow:32, TimeMid:16, TimeHigh:16, ClockHigh:8, ClockLow:8, Node:48>> = Rand,
    Uuid = new_uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node),
    set_variant(set_version(Uuid, 4)).

% @doc
%
% Generate a version 5 (named-based SHA1-hashed) universally unique
% identifier, as described in section 4.3 of RFC 4122.
%
% @since 0.2.0
%
-spec uuid5(uuid(), string() | binary()) -> uuid().
uuid5(#uuid{}=Namespace, Name) when is_list(Name); is_binary(Name) ->
    Digest = crypto:hash(sha, list_to_binary([
        Namespace#uuid.time_low,
        Namespace#uuid.time_mid,
        Namespace#uuid.time_high,
        Namespace#uuid.clock_high,
        Namespace#uuid.clock_low,
        Namespace#uuid.node,
        Name
    ])),
    <<TimeLow:32, TimeMid:16, TimeHigh:16, ClockHigh:8, ClockLow:8, Node:48, _:32>> = Digest,
    Uuid = new_uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node),
    set_variant(set_version(Uuid, 5)).

% @doc
%
% Decodes a string or binary representation of a UUID into a #uuid{} record. The
% input may be prefixed with "urn:uuid:" or enclosed in curly braces ({}). The
% dashes separating the parts are required, such that the value is separated
% into 8, 4, 4, 4, and 12 hexadecimal digits, respectively.
%
% @since 0.1.0
%
-spec decode(string() | binary()) -> uuid.
decode("urn:uuid:" ++ Input) ->
    decode(Input);
decode(Input) when is_list(Input) ->
    decode(list_to_binary(string:to_lower(Input)));
decode(<<"urn:uuid:", Input/bitstring>>) ->
    decode(Input);
decode(<<"{", Input:288/bitstring, "}">>) ->
    decode(Input);
decode(<<TimeLow:64/bitstring,  "-",
         TimeMid:32/bitstring,  "-",
         TimeHigh:32/bitstring, "-",
         ClockHigh:16/bitstring,
         ClockLow:16/bitstring, "-",
         Node:96/bitstring>>) ->
    new_uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node);
decode(_NotAUuid) ->
    error(badarg).

% @doc
%
% Encodes a given #uuid{} record into a binary string with dash
% (&lt;&lt;"-"&gt;&gt;) as the separator. If string output is desired, see
% to_string/1.
%
% @see inugami:to_string/1
% @since 0.1.0
%
-spec encode(uuid()) -> binary().
encode(#uuid{time_low=TimeLow, time_mid=TimeMid, time_high=TimeHigh,
             clock_high=ClockHigh, clock_low=ClockLow, node=Node}) ->
    TL = bin_to_hexbin(TimeLow),
    TM = bin_to_hexbin(TimeMid),
    TH = bin_to_hexbin(TimeHigh),
    CH = bin_to_hexbin(ClockHigh),
    CL = bin_to_hexbin(ClockLow),
    N = bin_to_hexbin(Node),
    <<TL/binary, "-", TM/binary, "-", TH/binary, "-", CH/binary, CL/binary, "-", N/binary>>.

% @doc
%
% Encodes a given #uuid{} record into a string, with the URN prefix.
%
% @since 0.1.0
%
-spec urn(uuid()) -> string().
urn(#uuid{}=Uuid) ->
    "urn:uuid:" ++ to_string(Uuid).

% @doc
%
% Return a string representation of the UUID with dash ("-") as the
% separator, as described in section 3 of RFC 4122.
%
% @see inugami:to_string/2
% @since 0.4.0
%
-spec to_string(uuid()) -> string().
to_string(#uuid{}=Uuid) ->
    to_string(dashed, Uuid).

% @doc
%
% Convert the UUID to a string, with the specified separator ("-" for
% 'dashed', "" for 'compact'). Note that anything other than dash
% separators does not conform to the UUID string format, as described in
% section 3 of RFC 4122.
%
% @see inugami:to_string/1
% @since 0.4.0
%
-spec to_string(dashed | compact | string(), uuid()) -> string().
to_string(dashed, #uuid{}=Uuid) ->
    to_string("-", Uuid);
to_string(compact, #uuid{}=Uuid) ->
    to_string("", Uuid);
to_string(Separator, #uuid{}=Uuid) ->
    Values = [
        bin_to_hexstr(Uuid#uuid.time_low),
        bin_to_hexstr(Uuid#uuid.time_mid),
        bin_to_hexstr(Uuid#uuid.time_high),
        bin_to_hexstr(Uuid#uuid.clock_high) ++ bin_to_hexstr(Uuid#uuid.clock_low),
        bin_to_hexstr(Uuid#uuid.node)
    ],
    string:join(Values, Separator).

% @doc
%
% Extract the version from the UUID as an 8-bit integer.
%
% @since 0.1.0
%
-spec get_version(uuid()) -> integer().
get_version(#uuid{time_high=TimeHigh}) ->
    <<V:4/bits, _R/bits>> = TimeHigh,
    <<N:8/integer>> = <<0:4, V/bits>>,
    N.

% @doc
%
% Set the version of the given UUID, returning the new record.
%
% @since 0.1.0
%
-spec set_version(uuid(), integer()) -> uuid().
set_version(#uuid{time_high=TimeHigh}=Uuid, Version) when is_binary(TimeHigh) ->
    <<_V:4/bits, R/bits>> = TimeHigh,
    Uuid#uuid{time_high = <<Version:4, R/bits>>}.

% @doc
%
% Extract the variant of the given UUID, returning an atom, such as
% variant_rfc4122, which is the default for every UUID generated by this
% module.
%
% @since 0.1.0
%
-spec get_variant(uuid()) -> variant_future | variant_microsoft | variant_rfc4122 | variant_ncs.
get_variant(#uuid{clock_high = <<V:3/bits, _R/bits>>}) when V == <<7:3>> ->
    variant_future;
get_variant(#uuid{clock_high = <<V:3/bits, _R/bits>>}) when V == <<6:3>> ->
    variant_microsoft;
get_variant(#uuid{clock_high = <<V:2/bits, _R/bits>>}) when V == <<2:2>> ->
    variant_rfc4122;
get_variant(#uuid{clock_high = <<V:1/bits, _R/bits>>}) when V == <<0:1>> ->
    variant_ncs.

% @doc
%
% Sets the variant bits as described in RFC 4122.
%
% @since 0.1.0
%
-spec set_variant(uuid()) -> uuid().
set_variant(#uuid{clock_high=ClockHigh}=Uuid) when is_binary(ClockHigh) ->
    <<_V:2/bits, R/bits>> = ClockHigh,
    Uuid#uuid{clock_high = <<2:2, R/bits>>}.

% @doc
%
% Convert a hexadecimal in binary form to a binary consisting of the values
% represented by the hexadecimal octets. For instance, the hexadecimal
% &lt;&lt;"01020304"&gt;&gt; becomes the bytes &lt;&lt;1,2,3,4&gt;&gt;.
%
% @since 0.1.0
%
-spec hexbin_to_bin(binary()) -> binary().
hexbin_to_bin(Bits) ->
    try hexstr_to_bin(binary_to_list(Bits)) of
        Result -> Result
    catch
        % translate the error in hexstr_to_bin/2 to what we would expect
        % if we were given some strange input, like "imnothex".
        error:{badmatch, _Reason} -> error(badarg)
    end.

% @doc
%
% Convert a binary to its hexadecimal representation in the form of a
% binary. For example, the bytes &lt;&lt;1,2,3,4&gt;&gt; become the
% hexadecimal &lt;&lt;"01020304"&gt;&gt;.
%
% @since 0.1.0
%
-spec bin_to_hexbin(binary()) -> binary().
bin_to_hexbin(Bin) ->
    list_to_binary(bin_to_hexstr(Bin)).

%
% The code below comes from Steve Vinoski, via a comment on this blog post:
% http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html
%

% @doc
%
% Convert a binary to a hexadecimal string. For example, the bytes
% &lt;&lt;1,2,3,4&gt;&gt; become the hexadecimal "01020304".
%
-spec bin_to_hexstr(binary()) -> string().
bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

% @doc
%
% Convert a hexadecimal in string form to a binary consisting of the values
% represented by the hexadecimal octets. For instance, the hexadecimal
% "01020304" becomes the bytes &lt;&lt;1,2,3,4&gt;&gt;.
%
-spec hexstr_to_bin(string()) -> binary().
hexstr_to_bin(S) ->
    hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    hexstr_to_bin(T, [V | Acc]).
