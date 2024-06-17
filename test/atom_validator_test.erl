%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(atom_validator_test).
-include_lib("eunit/include/eunit.hrl").

atom_validator_test() ->
    valid = term_validator:validate(yolo, atom),
    valid = term_validator:validate(yolo, {atom, []}),

    Format = {atom, []},
    {invalid, not_atom} = term_validator:validate(42, Format),
    {invalid, not_atom} = term_validator:validate("Hello world!", Format),

    valid = term_validator:validate("Hello world!", {atom, [allow_string]}),

    ok.

atom_validator_one_of_option_test() ->
    Format = {atom, [{one_of, [foo, bar]}]},
    valid = term_validator:validate(foo, Format),
    valid = term_validator:validate(bar, Format),
    {invalid, {not_one_of, [foo, bar]}} = term_validator:validate(quz, Format),

    ok.
