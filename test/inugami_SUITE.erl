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

-include("inugami.hrl").

all() ->
    [
        test_uuid4_gen,
        test_decode,
        test_encode,
        test_urn,
        test_version,
        test_variant,
        test_namespace_dns,
        test_namespace_url,
        test_namespace_oid,
        test_namespace_x500,
        test_bin_to_bitstring,
        test_bitstring_to_bin
    ].

test_uuid4_gen(_Config) ->
    Actual = inugami:encode(inugami:uuid4()),
    % Be explicit about our expectations rather than letting byte_size/1
    % raise a badarg error and make me confused.
    ?assert(is_bitstring(Actual)),
    ?assertEqual(36, byte_size(Actual)),
    ok.

test_decode(_Config) ->
    % Acceptable inputs
    Expected = inugami:uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:decode("urn:uuid:6ba7b810-9dad-11d1-80b4-00c04fd430c8")),
    ?assertEqual(Expected, inugami:decode("{6ba7b810-9dad-11d1-80b4-00c04fd430c8}")),
    ?assertEqual(Expected, inugami:decode("6ba7b810-9dad-11d1-80b4-00c04fd430c8")),
    ?assertEqual(Expected, inugami:decode("6BA7B810-9DAD-11D1-80B4-00C04FD430C8")),
    ?assertEqual(Expected, inugami:decode(<<"urn:uuid:6ba7b810-9dad-11d1-80b4-00c04fd430c8">>)),
    ?assertEqual(Expected, inugami:decode(<<"{6ba7b810-9dad-11d1-80b4-00c04fd430c8}">>)),
    ?assertEqual(Expected, inugami:decode(<<"6ba7b810-9dad-11d1-80b4-00c04fd430c8">>)),
    ?assertEqual(Expected, inugami:decode(<<"6BA7B810-9DAD-11D1-80B4-00C04FD430C8">>)),

    % Special "nil" UUID
    NilUuid = inugami:uuid(<<"00000000">>, <<"0000">>, <<"0000">>, <<"00">>, <<"00">>, <<"000000000000">>),
    ?assertEqual(NilUuid, inugami:decode(<<"00000000-0000-0000-0000-000000000000">>)),

    % Malformed inputs
    ?assertError(badarg, inugami:decode("foo-bar-not-a-uuid")),
    ?assertError(badarg, inugami:decode("ZZa7b810-9dZZ-11d1-80MM-00c0OOd430c8")),
    ok.

test_encode(_Config) ->
    % Acceptable inputs
    Input = inugami:uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(<<"6ba7b810-9dad-11d1-80b4-00c04fd430c8">>, inugami:encode(Input)),

    % Special "nil" UUID
    NilInput = inugami:uuid(<<"00000000">>, <<"0000">>, <<"0000">>, <<"00">>, <<"00">>, <<"000000000000">>),
    ?assertEqual(<<"00000000-0000-0000-0000-000000000000">>, inugami:encode(NilInput)),

    % Malformed inputs
    ?assertError(badarg, inugami:encode("not-a-uuid")),
    ok.

test_urn(_Config) ->
    Input = inugami:uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual("urn:uuid:6ba7b810-9dad-11d1-80b4-00c04fd430c8", inugami:urn(Input)),

    NilInput = inugami:uuid(<<"00000000">>, <<"0000">>, <<"0000">>, <<"00">>, <<"00">>, <<"000000000000">>),
    ?assertEqual("urn:uuid:00000000-0000-0000-0000-000000000000", inugami:urn(NilInput)),

    ?assertError(badarg, inugami:urn("not-a-uuid")),
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
    ct:log(default, 50, "UUID1 TimeHigh: ~w", [Uuid1#uuid.time_high]),
    ?assertEqual(1, inugami:get_version(Uuid1)),
    Uuid3 = inugami:decode("9073926b-929f-31c2-abc9-fad77ae3e8eb"),
    ?assertEqual(3, inugami:get_version(Uuid3)),
    Uuid4 = inugami:decode("791ebc59-c353-468e-9115-af55da5fda6f"),
    ?assertEqual(4, inugami:get_version(Uuid4)),
    Uuid5 = inugami:decode("cfbff0d1-9375-5685-968c-48ce8b15ae17"),
    ?assertEqual(5, inugami:get_version(Uuid5)),
    ok.

test_variant(_Config) ->
    ?assertEqual(variant_rfc4122, inugami:get_variant(inugami:uuid4())),
    ?assertError(badarg, inugami:get_variant("not-a-uuid")),
    ok.

test_namespace_dns(_Config) ->
    Expected = inugami:uuid(<<"6ba7b810">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:namespace_dns()),
    ok.

test_namespace_url(_Config) ->
    Expected = inugami:uuid(<<"6ba7b811">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:namespace_url()),
    ok.

test_namespace_oid(_Config) ->
    Expected = inugami:uuid(<<"6ba7b812">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:namespace_oid()),
    ok.

test_namespace_x500(_Config) ->
    Expected = inugami:uuid(<<"6ba7b814">>, <<"9dad">>, <<"11d1">>, <<"80">>, <<"b4">>, <<"00c04fd430c8">>),
    ?assertEqual(Expected, inugami:namespace_x500()),
    ok.

% TODO: generate a uuid, then extract parts and compare to expectations
%     - MAC address
%     - time stamp
%     - version
% TODO: how to check that uuid has valid timestamp
%     - get time1
%     - generate uuid
%     - get time2
%     - extract time for uuid
%     - verify extracted time is between time1 and time2

test_bin_to_bitstring(_Config) ->
    ?assertEqual(<<"80943206">>, inugami:bin_to_bitstring(<<128,148,50,6>>)),
    ?assertEqual(<<"00c04fd430c8">>, inugami:bin_to_bitstring(<<0,192,79,212,48,200>>)),
    ?assertEqual(<<"00">>, inugami:bin_to_bitstring(<<0>>)),
    ?assertEqual(<<"11d1">>, inugami:bin_to_bitstring(<<17, 209>>)),
    % test round trip
    ?assertEqual(<<17, 209>>, inugami:bitstring_to_bin(inugami:bin_to_bitstring(<<17, 209>>))),
    ok.

test_bitstring_to_bin(_Config) ->
    ?assertEqual(<<128,148,50,6>>, inugami:bitstring_to_bin(<<"80943206">>)),
    ?assertEqual(<<0,192,79,212,48,200>>, inugami:bitstring_to_bin(<<"00c04fd430c8">>)),
    ?assertEqual(<<0>>, inugami:bitstring_to_bin(<<"00">>)),
    ?assertEqual(<<17, 209>>, inugami:bitstring_to_bin(<<"11d1">>)),
    % test round trip
    ?assertEqual(<<"11d1">>, inugami:bin_to_bitstring(inugami:bitstring_to_bin(<<"11d1">>))),
    ok.
