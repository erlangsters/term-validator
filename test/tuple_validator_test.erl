%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(tuple_validator_test).
-include_lib("eunit/include/eunit.hrl").

tuple_validator_test() ->
    valid = term_validator:validate({}, {tuple, [{elements, []}]}),
    {missing_options, [elements]} = term_validator:validate({}, tuple),
    {missing_options, [elements]} = term_validator:validate({}, {tuple, []}),

    Format = {tuple, [{elements, []}]},
    {invalid, not_tuple} = term_validator:validate(yolo, Format),
    {invalid, not_tuple} = term_validator:validate(42, Format),
    {invalid, not_tuple} = term_validator:validate("Hello world!", Format),
    {invalid, not_tuple} = term_validator:validate([], Format),
    valid = term_validator:validate({}, Format),
    {invalid, not_tuple} = term_validator:validate(#{}, Format),

    ok.

tuple_validator_elements_test() ->
    Format = {tuple, [{elements, [bool, number, string]}]},
    Term = {true, 42, "Hello world!"},
    valid = term_validator:validate(Term, Format),

    {invalid, {elements, [{1, not_string}, {3, not_bool}]}} =
        term_validator:validate(Term, {tuple, [{elements, [string, number, bool]}]}),

    % Test when the tuple has more and less elements than expected.
    {invalid, too_many_elements} =
        term_validator:validate({true, 42, "Hello world!", 0.0}, Format),
    {invalid, too_few_elements} =
        term_validator:validate({true, 42}, Format),

    ok.
