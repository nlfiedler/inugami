%% -*- coding: utf-8 -*-
%%
%% Copyright 2016 Nathan Fiedler. All rights reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.
%%

-module(inugami_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        test_uuid_gen,
        test_uuid1_gen,
        test_uuid3_gen,
        test_uuid4_gen,
        test_uuid5_gen,
        test_decode,
        test_encode,
        test_urn,
        test_to_string,
        test_version,
        test_variant,
        test_namespace_dns,
        test_namespace_url,
        test_namespace_oid,
        test_namespace_x500,
        test_bin_to_hexbin,
        test_hexbin_to_bin
    ].

test_uuid_gen(_Config) ->
    Expected = <<"00000000-0000-0000-0000-000000000000">>,
    ?assertEqual(Expected, inugami:encode(inugami:new_uuid(0, 0, 0, 0, 0, 0))),
    ?assertEqual(Expected, inugami:encode(inugami:new_uuid(
        <<"00000000">>, <<"0000">>, <<"0000">>, <<"00">>, <<"00">>, <<"000000000000">>))),
    ok.

test_uuid1_gen(_Config) ->
    Actual = inugami:encode(inugami:uuid1()),
    % Be explicit about our expectations rather than letting byte_size/1
    % raise a badarg error and make me confused.
    ?assert(is_binary(Actual)),
    ?assertEqual(36, byte_size(Actual)),
    ?assertEqual(1, inugami:get_version(inugami:uuid1())),

    % Ensure timestamp of version 1 UUID is correct.
    TimeBefore = os:timestamp(),
    timer:sleep(100),
    UuidTime = inugami:get_timestamp(inugami:uuid1()),
    timer:sleep(100),
    TimeAfter = os:timestamp(),
    ?assert(TimeBefore < UuidTime),
    ?assert(UuidTime < TimeAfter),

    % Ensure node of version 1 UUID is correct.
    {ok, Interfaces} = inet:getifaddrs(),
    HwAddr = find_hwaddr(Interfaces),
    ByteList = binary_to_list(HwAddr),
    HexList = [lists:flatten(io_lib:format("~2.16.0b", [X])) || X <- ByteList],
    Expected = list_to_binary(string:join(HexList, ":")),
    ?assertEqual(Expected, inugami:get_node(inugami:uuid1())),

    % Generate a bunch of uuid1 values and ensure uniqueness. Also tried
    % 100,000 without any problem, but it takes noticeably longer for the
    % test to finish.
    InputValues = [inugami:uuid1() || _ <- lists:seq(1, 10000)],
    UniqueValues = lists:usort(InputValues),
    ?assertEqual(length(InputValues), length(UniqueValues)),
    ok.

test_uuid3_gen(_Config) ->
    % Expected values produced using Python uuid module.
    ?assertEqual(<<"9073926b-929f-31c2-abc9-fad77ae3e8eb">>,
                 inugami:encode(inugami:uuid3(inugami:namespace_dns(), "example.com"))),
    ?assertEqual(<<"9073926b-929f-31c2-abc9-fad77ae3e8eb">>,
                 inugami:encode(inugami:uuid3(inugami:namespace_dns(), <<"example.com">>))),
    ?assertEqual(<<"66402c62-d8b5-3acc-828d-1791e7f2862d">>,
                 inugami:encode(inugami:uuid3(inugami:namespace_url(), "http://www.example.com/path"))),
    ?assertEqual(<<"8f9c88e1-3faa-3b09-93ed-0cba78ee6dae">>,
                 inugami:encode(inugami:uuid3(inugami:namespace_oid(), "1.3.6.1.4.1.1973"))),
    ?assertEqual(<<"bdb3be56-9952-3e2c-a745-2bd15f9680c7">>,
                 inugami:encode(inugami:uuid3(inugami:namespace_x500(), "cn=Joe User, o=Example, c=US"))),
    ?assertEqual(3, inugami:get_version(inugami:uuid3(inugami:namespace_dns(), "example.com"))),

    % Special "nil" UUID
    ?assertEqual(<<"5205aa98-add7-3479-85a2-12aa09b46a70">>,
                 inugami:encode(inugami:uuid3(inugami:nil(), "foo bar baz quux"))),

    % Malformed inputs
    ?assertError(function_clause, inugami:uuid3("foo-bar-not-a-uuid", "whatever")),
    ?assertError(function_clause, inugami:uuid3(<<"foo-bar-not-a-uuid">>, "whatever")),
    ?assertError(function_clause, inugami:uuid3(123456, "whatever")),
    ?assertError(function_clause, inugami:uuid3(inugami:nil(), 123456)),
    ok.

test_uuid4_gen(_Config) ->
    Actual = inugami:encode(inugami:uuid4()),
    % Be explicit about our expectations rather than letting byte_size/1
    % raise a badarg error and make me confused.
    ?assert(is_binary(Actual)),
    ?assertEqual(36, byte_size(Actual)),
    ?assertEqual(4, inugami:get_version(inugami:uuid4())),

    % Generate a bunch of uuid4 values and ensure uniqueness. Also tried
    % 1,000,000 without any problem, but it takes noticeably longer for the
    % test to finish.
    InputValues = [inugami:uuid4() || _ <- lists:seq(1, 100000)],
    UniqueValues = lists:usort(InputValues),
    ?assertEqual(length(InputValues), length(UniqueValues)),
    ok.

test_uuid5_gen(_Config) ->
    % Expected values produced using Python uuid module.
    ?assertEqual(<<"cfbff0d1-9375-5685-968c-48ce8b15ae17">>,
                 inugami:encode(inugami:uuid5(inugami:namespace_dns(), "example.com"))),
    ?assertEqual(<<"cfbff0d1-9375-5685-968c-48ce8b15ae17">>,
                 inugami:encode(inugami:uuid5(inugami:namespace_dns(), <<"example.com">>))),
    ?assertEqual(<<"45dbc6a2-a77d-5587-85c4-8663314bc20f">>,
                 inugami:encode(inugami:uuid5(inugami:namespace_url(), "http://www.example.com/path"))),
    ?assertEqual(<<"8f808e20-d3c6-5da5-85de-ed03829ae3c8">>,
                 inugami:encode(inugami:uuid5(inugami:namespace_oid(), "1.3.6.1.4.1.1973"))),
    ?assertEqual(<<"120689c6-401d-5a39-915c-15462164d3fc">>,
                 inugami:encode(inugami:uuid5(inugami:namespace_x500(), "cn=Joe User, o=Example, c=US"))),
    ?assertEqual(5, inugami:get_version(inugami:uuid5(inugami:namespace_dns(), "example.com"))),

    % Special "nil" UUID
    ?assertEqual(<<"f82da04c-2517-594f-9fc3-170c63914537">>,
                 inugami:encode(inugami:uuid5(inugami:nil(), "foo bar baz quux"))),

    % Malformed inputs
    ?assertError(function_clause, inugami:uuid5("foo-bar-not-a-uuid", "whatever")),
    ?assertError(function_clause, inugami:uuid5(<<"foo-bar-not-a-uuid">>, "whatever")),
    ?assertError(function_clause, inugami:uuid5(123456, "whatever")),
    ?assertError(function_clause, inugami:uuid5(inugami:nil(), 123456)),
    ok.

test_decode(_Config) ->
    % Acceptable inputs
    Expected = inugami:new_uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:decode("urn:uuid:6ba7b810-9dad-11d1-80b4-00c04fd430c8")),
    ?assertEqual(Expected, inugami:decode("{6ba7b810-9dad-11d1-80b4-00c04fd430c8}")),
    ?assertEqual(Expected, inugami:decode("6ba7b810-9dad-11d1-80b4-00c04fd430c8")),
    ?assertEqual(Expected, inugami:decode("6BA7B810-9DAD-11D1-80B4-00C04FD430C8")),
    ?assertEqual(Expected, inugami:decode(<<"urn:uuid:6ba7b810-9dad-11d1-80b4-00c04fd430c8">>)),
    ?assertEqual(Expected, inugami:decode(<<"{6ba7b810-9dad-11d1-80b4-00c04fd430c8}">>)),
    ?assertEqual(Expected, inugami:decode(<<"6ba7b810-9dad-11d1-80b4-00c04fd430c8">>)),
    ?assertEqual(Expected, inugami:decode(<<"6BA7B810-9DAD-11D1-80B4-00C04FD430C8">>)),

    % Special "nil" UUID
    ?assertEqual(inugami:nil(), inugami:decode(<<"00000000-0000-0000-0000-000000000000">>)),

    % Malformed inputs
    ?assertError(badarg, inugami:decode(<<"foo-bar-not-a-uuid">>)),
    ?assertError(badarg, inugami:decode(<<"ZZa7b810-9dZZ-11d1-80MM-00c0OOd430c8">>)),
    ?assertError(badarg, inugami:decode(<<"6ba7b810-9dad-11d1-80b4-00c04fd430">>)),
    ?assertError(badarg, inugami:decode(<<"6ba7b810-9dad-11d1-80b4-00c04fd430c89a">>)),
    ?assertError(badarg, inugami:decode(<<"6ba7b81-09dad-11d180b400-c04fd430c8">>)),
    ?assertError(badarg, inugami:decode(<<"6ba7b810+9dad+11d1+80b4+00c04fd430c8">>)),
    ok.

test_encode(_Config) ->
    % Acceptable inputs
    Input = inugami:new_uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(<<"6ba7b810-9dad-11d1-80b4-00c04fd430c8">>, inugami:encode(Input)),

    % Special "nil" UUID
    ?assertEqual(<<"00000000-0000-0000-0000-000000000000">>, inugami:encode(inugami:nil())),

    % Malformed inputs
    ?assertError(function_clause, inugami:encode("not-a-uuid")),
    ok.

test_urn(_Config) ->
    Input = inugami:new_uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual("urn:uuid:6ba7b810-9dad-11d1-80b4-00c04fd430c8", inugami:urn(Input)),

    % Special "nil" UUID
    ?assertEqual("urn:uuid:00000000-0000-0000-0000-000000000000", inugami:urn(inugami:nil())),

    % Malformed inputs
    ?assertError(function_clause, inugami:urn("not-a-uuid")),
    ok.

test_to_string(_Config) ->
    Input = inugami:new_uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual("6ba7b810-9dad-11d1-80b4-00c04fd430c8", inugami:to_string(Input)),
    ?assertEqual("6ba7b810-9dad-11d1-80b4-00c04fd430c8", inugami:to_string(dashed, Input)),
    ?assertEqual("6ba7b8109dad11d180b400c04fd430c8", inugami:to_string(compact, Input)),
    ?assertEqual("6ba7b810+9dad+11d1+80b4+00c04fd430c8", inugami:to_string("+", Input)),
    ok.

test_version(_Config) ->
    %
    % UUIDs generated in inugami...
    %
    ?assertEqual(4, inugami:get_version(inugami:uuid4())),
    %
    % UUIDs generated using python3...
    %
    Uuid1 = inugami:decode("80943206-7160-11e6-8b6f-3c07547e18a6"),
    ?assertEqual(1, inugami:get_version(Uuid1)),
    Uuid3 = inugami:decode("9073926b-929f-31c2-abc9-fad77ae3e8eb"),
    ?assertEqual(3, inugami:get_version(Uuid3)),
    Uuid4 = inugami:decode("791ebc59-c353-468e-9115-af55da5fda6f"),
    ?assertEqual(4, inugami:get_version(Uuid4)),
    Uuid5 = inugami:decode("cfbff0d1-9375-5685-968c-48ce8b15ae17"),
    ?assertEqual(5, inugami:get_version(Uuid5)),

    % Invalid inputs
    ?assertError(function_clause, inugami:get_version("not-a-uuid")),
    ok.

test_variant(_Config) ->
    % The default variant for this library is RFC 4122
    ?assertEqual(variant_rfc4122, inugami:get_variant(inugami:uuid4())),

    % Normal cases
    ?assertEqual(variant_ncs, inugami:get_variant(inugami:decode(<<"00000000-0000-0000-0000-000000000000">>))),
    ?assertEqual(variant_rfc4122, inugami:get_variant(inugami:decode(<<"00000000-0000-0000-8000-000000000000">>))),
    ?assertEqual(variant_microsoft, inugami:get_variant(inugami:decode(<<"00000000-0000-0000-c000-000000000000">>))),
    ?assertEqual(variant_future, inugami:get_variant(inugami:decode(<<"00000000-0000-0000-e000-000000000000">>))),

    % Invalid inputs
    ?assertError(function_clause, inugami:get_variant("not-a-uuid")),
    ok.

test_namespace_dns(_Config) ->
    Expected = inugami:new_uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:namespace_dns()),
    ok.

test_namespace_url(_Config) ->
    Expected = inugami:new_uuid(<<"6ba7b811">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:namespace_url()),
    ok.

test_namespace_oid(_Config) ->
    Expected = inugami:new_uuid(<<"6ba7b812">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:namespace_oid()),
    ok.

test_namespace_x500(_Config) ->
    Expected = inugami:new_uuid(<<"6ba7b814">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:namespace_x500()),
    ok.

test_bin_to_hexbin(_Config) ->
    ?assertEqual(<<"80943206">>, inugami:bin_to_hexbin(<<128,148,50,6>>)),
    ?assertEqual(<<"00c04fd430c8">>, inugami:bin_to_hexbin(<<0,192,79,212,48,200>>)),
    ?assertEqual(<<"00">>, inugami:bin_to_hexbin(<<0>>)),
    ?assertEqual(<<"11d1">>, inugami:bin_to_hexbin(<<17, 209>>)),
    % test round trip
    ?assertEqual(<<17, 209>>, inugami:hexbin_to_bin(inugami:bin_to_hexbin(<<17, 209>>))),
    ok.

test_hexbin_to_bin(_Config) ->
    ?assertEqual(<<128,148,50,6>>, inugami:hexbin_to_bin(<<"80943206">>)),
    ?assertEqual(<<0,192,79,212,48,200>>, inugami:hexbin_to_bin(<<"00c04fd430c8">>)),
    ?assertEqual(<<0>>, inugami:hexbin_to_bin(<<"00">>)),
    ?assertEqual(<<17, 209>>, inugami:hexbin_to_bin(<<"11d1">>)),
    % test round trip
    ?assertEqual(<<"11d1">>, inugami:bin_to_hexbin(inugami:hexbin_to_bin(<<"11d1">>))),
    ok.

% Find a suitable network address, generating a random value if necessary.
% Duplicate code from the module under test, but this is ostensibly an
% internal function, so double-check our work by doing it twice?
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
