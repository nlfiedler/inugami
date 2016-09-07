%% -*- coding: utf-8 -*-
%%
%% Copyright 2016 Nathan Fiedler. All rights reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.
%%

-module(inugami).

-export([nil/0, uuid/6, uuid1/0, uuid3/2, uuid4/0, uuid5/2]).
-export([decode/1, encode/1, urn/1]).
-export([get_version/1, set_version/2]).
-export([get_variant/1, set_variant/1]).
-export([namespace_dns/0, namespace_url/0, namespace_oid/0, namespace_x500/0]).
-export([bitstring_to_bin/1, bin_to_bitstring/1]).

-include("inugami.hrl").

% The difference in 100-nanosecond intervals between the UUID epoch
% (15 October 1582) and the Unix epoch (1 January 1970).
-define(NANOSECOND_INTERVALS_OFFSET, 122192928000000000).

namespace_dns() -> decode(<<"6ba7b810-9dad-11d1-80b4-00c04fd430c8">>).
namespace_url() -> decode(<<"6ba7b811-9dad-11d1-80b4-00c04fd430c8">>).
namespace_oid() -> decode(<<"6ba7b812-9dad-11d1-80b4-00c04fd430c8">>).
namespace_x500() -> decode(<<"6ba7b814-9dad-11d1-80b4-00c04fd430c8">>).

% Construct a UUID from the given parts, either bitstrings or integers.
%
% e.g. inugami:uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>,
%                   <<"80">>, <<"b4">>, <<"00c04fd430c8">>).
% e.g. inugami:uuid(3485462737, 37749, 13957, 86, 140, 80051933916695).
uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node) when is_bitstring(TimeLow) ->
    #uuid{
        time_low = inugami:bitstring_to_bin(TimeLow),
        time_mid = inugami:bitstring_to_bin(TimeMid),
        time_high = inugami:bitstring_to_bin(TimeHigh),
        clock_high = inugami:bitstring_to_bin(ClockHigh),
        clock_low = inugami:bitstring_to_bin(ClockLow),
        node = inugami:bitstring_to_bin(Node)
    };
uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node) when is_integer(TimeLow) ->
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

% Construct the nil UUID as described in section 4.1.7 of RFC 4122.
nil() -> uuid(0, 0, 0, 0, 0, 0).

% Generate a version 1 (time-based) universally unique identifier, as
% described in section 4.2.2 of RFC 4122.
uuid1() ->
    <<TimeHigh:12, TimeMid:16, TimeLow:32>> = timestamp(),
    % No real clock sequence, just a random number, per section 4.1.5 of
    % RFC 4122.
    <<ClockHigh:6, ClockLow:8, _R/bits>> = crypto:strong_rand_bytes(2),
    % Make everything an integer so uuid/6 has an easy time.
    Node = binary:decode_unsigned(get_node()),
    Uuid = uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node),
    set_variant(set_version(Uuid, 1)).

% Return a 60-bit timestamp value suitable for version 1 UUID. Uses the
% current system time and converts to the number of 100-nanosecond
% intervals since the UUID epoch (15 October 1582).
timestamp() ->
    % Convert the Unix epoch microseconds to nanoseconds (1 MS = 1000 NS)
    % and divide that by 100 to get the number of 100-second intervals
    % since the UUID epoch. Or just multiply by 10 because math.
    {MegaSeconds, Seconds, MicroSeconds} = os:timestamp(),
    UnixEpochNanos = MegaSeconds * 1000000000000 + Seconds * 1000000 + MicroSeconds,
    Timestamp = ?NANOSECOND_INTERVALS_OFFSET + UnixEpochNanos * 10,
    <<Timestamp:60>>.

% Find the network hardware address given the set of available interfaces.
% Avoid the loopback interface as its address is fixed. If no suitable
% address can be found, generate one with the multicast bit set to avoid
% conflict with addresses obtained from network cards. See section 4.1.6 of
% RFC 4122.
get_node() ->
    {ok, Interfaces} = inet:getifaddrs(),
    find_hwaddr(Interfaces).

% Find a suitable network address, generating a random value if necessary.
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

% Generate a version 3 (named-based MD5-hashed) universally unique
% identifier, as described in section 4.3 of RFC 4122.
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
    Uuid = uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node),
    set_variant(set_version(Uuid, 3)).

% Generate a version 4 (random) universally unique identifier, as described
% in section 4.4 of RFC 4122.
uuid4() ->
    Rand = crypto:strong_rand_bytes(16),
    <<TimeLow:32, TimeMid:16, TimeHigh:16, ClockHigh:8, ClockLow:8, Node:48>> = Rand,
    Uuid = uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node),
    set_variant(set_version(Uuid, 4)).

% Generate a version 5 (named-based SHA1-hashed) universally unique
% identifier, as described in section 4.3 of RFC 4122.
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
    Uuid = uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node),
    set_variant(set_version(Uuid, 5)).

% Decodes a string or binary representation of a UUID into a #uuid{} record.
decode("urn:uuid:" ++ Input) ->
    decode(Input);
decode(Input) when is_list(Input) ->
    decode(list_to_bitstring(string:to_lower(Input)));
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
    uuid(TimeLow, TimeMid, TimeHigh, ClockHigh, ClockLow, Node);
decode(_NotAUuid) ->
    error(badarg).

% Encodes a given #uuid{} record into a binary string.
encode(#uuid{time_low=TimeLow, time_mid=TimeMid, time_high=TimeHigh,
             clock_high=ClockHigh, clock_low=ClockLow, node=Node}) ->
    TL = bin_to_bitstring(TimeLow),
    TM = bin_to_bitstring(TimeMid),
    TH = bin_to_bitstring(TimeHigh),
    CH = bin_to_bitstring(ClockHigh),
    CL = bin_to_bitstring(ClockLow),
    N = bin_to_bitstring(Node),
    <<TL/binary, "-", TM/binary, "-", TH/binary, "-", CH/binary, CL/binary, "-", N/binary>>;
encode(_NotAUuid) ->
    error(badarg).

% Encodes a given #uuid{} record into a string, with the URN prefix.
urn(Input) ->
    "urn:uuid:" ++ binary_to_list(encode(Input)).

% Extract the version from the UUID as an 8-bit integer.
get_version(#uuid{time_high=TimeHigh}) ->
    <<V:4/bits, _R/bits>> = TimeHigh,
    <<N:8/integer>> = <<0:4, V/bits>>,
    N.

% Set the version of the given UUID, returning the new record.
set_version(#uuid{time_high=TimeHigh}=Uuid, Version) when is_binary(TimeHigh) ->
    <<_V:4/bits, R/bits>> = TimeHigh,
    Uuid#uuid{time_high = <<Version:4, R/bits>>}.

% Extract the variant of the given UUID, returning an atom, such as
% variant_rfc4122, which is the default for every UUID generated by this
% module.
get_variant(#uuid{clock_high = <<V:3/bits, _R/bits>>}) when V == <<7:3>> ->
    variant_future;
get_variant(#uuid{clock_high = <<V:3/bits, _R/bits>>}) when V == <<6:3>> ->
    variant_microsoft;
get_variant(#uuid{clock_high = <<V:2/bits, _R/bits>>}) when V == <<2:2>> ->
    variant_rfc4122;
get_variant(#uuid{clock_high = <<V:1/bits, _R/bits>>}) when V == <<0:1>> ->
    variant_ncs;
get_variant(_Uuid) ->
    error(badarg).

% Sets the variant bits as described in RFC 4122.
set_variant(#uuid{clock_high=ClockHigh}=Uuid) when is_binary(ClockHigh) ->
    <<_V:2/bits, R/bits>> = ClockHigh,
    Uuid#uuid{clock_high = <<2:2, R/bits>>}.

% Convert a bitstring representation of a hexadecimal string (e.g.
% <<"80943206">>) to a proper binary (e.g. <<128,148,50,6>>).
bitstring_to_bin(Bits) ->
    try hexstr_to_bin(bitstring_to_list(Bits)) of
        Result -> Result
    catch
        % translate the error in hexstr_to_bin/2 to what we would expect
        % if we were given some strange input, like "imnothex".
        error:{badmatch, _Reason} -> error(badarg)
    end.

% Convert a binary (e.g. <<128,148,50,6>>) to its hexadecimal string
% representation (e.g. <<"80943206">>).
bin_to_bitstring(Bin) ->
    list_to_bitstring(bin_to_hexstr(Bin)).

%
% The code below comes from Steve Vinoski, via a comment on this blog post:
% http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html
%

% Convert a binary to a hexadecimal string.
bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Bin)]).

% Convert a hexadecimal string to a binary.
hexstr_to_bin(S) ->
    hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    hexstr_to_bin(T, [V | Acc]).
