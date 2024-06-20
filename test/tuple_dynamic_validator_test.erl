%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(tuple_dynamic_validator_test).
-include_lib("eunit/include/eunit.hrl").

tuple_dynamic_validator_test() ->
    valid = term_validator:validate({}, tuple_dynamic),
    valid = term_validator:validate({}, {tuple_dynamic, []}),

    Format = {tuple_dynamic, []},
    {invalid, not_tuple} = term_validator:validate(yolo, Format),
    {invalid, not_tuple} = term_validator:validate(42, Format),
    {invalid, not_tuple} = term_validator:validate("Hello world!", Format),
    {invalid, not_tuple} = term_validator:validate([], Format),
    valid = term_validator:validate({}, Format),
    {invalid, not_tuple} = term_validator:validate(#{}, Format),

    valid = term_validator:validate({true}, Format),
    valid = term_validator:validate({true, 42}, Format),
    valid = term_validator:validate({true, 42, "Hello world!"}, Format),

    ok.

tuple_dynamic_validator_element_test() ->
    Format = {tuple_dynamic, [{element, any}]},
    Term = {true, 42, "Hello world!"},
    valid = term_validator:validate(Term, Format),

    {invalid, {elements, [{2, not_bool}, {3, not_bool}]}} =
        term_validator:validate(Term, {tuple_dynamic, [{element, bool}]}),

    {invalid, {elements, [{1, not_number}, {3, not_number}]}} =
        term_validator:validate(Term, {tuple_dynamic, [{element, number}]}),

    {invalid, {elements, [{1, not_string}, {2, not_string}]}} =
        term_validator:validate(Term, {tuple_dynamic, [{element, string}]}),

    ElementFormat = {any_of, [bool, number, string]},
    valid = term_validator:validate(Term, {tuple_dynamic, [{element, ElementFormat}]}),

    ok.

tuple_dynamic_validator_length_test() ->
    Format1 = {tuple_dynamic, [{length, {2, 4}}]},
    Format2 = {tuple_dynamic, [{length, {{2, inclusive}, 4}}]},
    Format3 = {tuple_dynamic, [{length, {{2, exclusive}, 4}}]},
    Format4 = {tuple_dynamic, [{length, {2, {4, inclusive}}}]},
    Format5 = {tuple_dynamic, [{length, {2, {4, exclusive}}}]},

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} = term_validator:validate({1}, Format1),
    valid = term_validator:validate({1, 2}, Format1),
    valid = term_validator:validate({1, 2, 3}, Format1),
    valid = term_validator:validate({1, 2, 3, 4}, Format1),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} = term_validator:validate({1, 2, 3, 4, 5}, Format1),

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} = term_validator:validate({1}, Format2),
    valid = term_validator:validate({1, 2}, Format2),
    valid = term_validator:validate({1, 2, 3}, Format2),
    valid = term_validator:validate({1, 2, 3, 4}, Format2),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} = term_validator:validate({1, 2, 3, 4, 5}, Format2),

    {invalid, {length, {must_be_strictly_greater_than, 2}}} = term_validator:validate({1}, Format3),
    {invalid, {length, {must_be_strictly_greater_than, 2}}} = term_validator:validate({1, 2}, Format3),
    valid = term_validator:validate({1, 2, 3}, Format3),
    valid = term_validator:validate({1, 2, 3, 4}, Format3),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} = term_validator:validate({1, 2, 3, 4, 5}, Format3),

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} = term_validator:validate({1}, Format4),
    valid = term_validator:validate({1, 2}, Format4),
    valid = term_validator:validate({1, 2, 3}, Format4),
    valid = term_validator:validate({1, 2, 3, 4}, Format4),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} = term_validator:validate({1, 2, 3, 4, 5}, Format4),

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} = term_validator:validate({1}, Format5),
    valid = term_validator:validate({1, 2}, Format5),
    valid = term_validator:validate({1, 2, 3}, Format5),
    {invalid, {length, {must_be_strictly_less_than, 4}}} = term_validator:validate({1, 2, 3, 4}, Format5),
    {invalid, {length, {must_be_strictly_less_than, 4}}} = term_validator:validate({1, 2, 3, 4, 5}, Format5),

    ok.

tuple_dynamic_validator_length_min_test() ->
    Format1 = {tuple_dynamic, [{length, {3, no_max}}]},
    Format2 = {tuple_dynamic, [{length, {{3, inclusive}, no_max}}]},
    Format3 = {tuple_dynamic, [{length, {{3, exclusive}, no_max}}]},

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} = term_validator:validate({1, 2}, Format1),
    valid = term_validator:validate({1, 2, 3}, Format1),

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} = term_validator:validate({1, 2}, Format2),
    valid = term_validator:validate({1, 2, 3}, Format2),

    {invalid, {length, {must_be_strictly_greater_than, 3}}} = term_validator:validate({1, 2, 3}, Format3),
    valid = term_validator:validate({1, 2, 3, 4}, Format3),

    ok.

tuple_dynamic_validator_length_max_test() ->
    Format1 = {tuple_dynamic, [{length, {no_min, 3}}]},
    Format2 = {tuple_dynamic, [{length, {no_min, {3, inclusive}}}]},
    Format3 = {tuple_dynamic, [{length, {no_min, {3, exclusive}}}]},

    valid = term_validator:validate({1, 2, 3}, Format1),
    {invalid, {length, {must_be_less_or_equal_to, 3}}} = term_validator:validate({1, 2, 3, 4}, Format1),

    valid = term_validator:validate({1, 2, 3}, Format2),
    {invalid, {length, {must_be_less_or_equal_to, 3}}} = term_validator:validate({1, 2, 3, 4}, Format2),

    valid = term_validator:validate({1, 2}, Format3),
    {invalid, {length, {must_be_strictly_less_than, 3}}} = term_validator:validate({1, 2, 3}, Format3),

    ok.

tuple_dynamic_validator_min_test() ->
    Format1 = {tuple_dynamic, [{min, 3}]},
    Format2 = {tuple_dynamic, [{min, {3, inclusive}}]},
    Format3 = {tuple_dynamic, [{min, {3, exclusive}}]},

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} =
        term_validator:validate({1, 2}, Format1),
    valid = term_validator:validate({1, 2, 3}, Format1),

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} =
        term_validator:validate({1, 2}, Format2),
    valid = term_validator:validate({1, 2, 3}, Format2),

    {invalid, {length, {must_be_strictly_greater_than, 3}}} =
        term_validator:validate({1, 2, 3}, Format3),
    valid = term_validator:validate({1, 2, 3, 4}, Format3),

    % Test if the 'min' option becomes an invalid when used with the 'length'
    % option.
    Format4 = {tuple_dynamic, [{length, {3, no_max}}, {min, 3}]},
    {invalid_options, [min]} = term_validator:validate({1, 2, 3}, Format4),

    ok.

tuple_dynamic_validator_max_test() ->
    Format1 = {tuple_dynamic, [{max, 3}]},
    Format2 = {tuple_dynamic, [{max, {3, inclusive}}]},
    Format3 = {tuple_dynamic, [{max, {3, exclusive}}]},

    {invalid, {length, {must_be_less_or_equal_to, 3}}} =
        term_validator:validate({1, 2, 3, 4}, Format1),
    valid = term_validator:validate({1, 2, 3}, Format1),

    {invalid, {length, {must_be_less_or_equal_to, 3}}} =
        term_validator:validate({1, 2, 3, 4}, Format2),
    valid = term_validator:validate({1, 2, 3}, Format2),

    {invalid, {length, {must_be_strictly_less_than, 3}}} =
        term_validator:validate({1, 2, 3}, Format3),
    valid = term_validator:validate({1, 2}, Format3),

    % Test if the 'max' option becomes an invalid when used with the 'length'
    % option.
    Format4 = {tuple_dynamic, [{length, {no_min, 3}}, {max, 3}]},
    {invalid_options, [max]} = term_validator:validate({1, 2, 3}, Format4),

    ok.
