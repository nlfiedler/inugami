%% -*- coding: utf-8 -*-
%%
%% Copyright 2016 Nathan Fiedler. All rights reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.
%%

% A UUID is simply a tuple of binaries.
-record(uuid, {time_low   :: binary,
               time_mid   :: binary,
               time_high  :: binary,
               clock_high :: binary,
               clock_low  :: binary,
               node       :: binary}).
