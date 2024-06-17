%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(bool_validator_test).
-include_lib("eunit/include/eunit.hrl").

bool_validator_test() ->
    valid = term_validator:validate(false, bool),
    valid = term_validator:validate(true, {bool, []}),

    Format = {bool, []},
    {invalid, not_bool} = term_validator:validate(42, Format),
    {invalid, not_bool} = term_validator:validate("Hello world!", Format),

    valid = term_validator:validate(0, {bool, [allow_number]}),
    valid = term_validator:validate(-1, {bool, [allow_number]}),

    ok.
