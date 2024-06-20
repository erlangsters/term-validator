%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(map_dynamic_validator_test).
-include_lib("eunit/include/eunit.hrl").

map_dynamic_validator_test() ->
    valid = term_validator:validate(#{}, map_dynamic),
    valid = term_validator:validate(#{}, {map_dynamic, []}),

    Format = {map_dynamic, []},
    {invalid, not_map} = term_validator:validate(yolo, Format),
    {invalid, not_map} = term_validator:validate(42, Format),
    {invalid, not_map} = term_validator:validate("Hello world!", Format),
    {invalid, not_map} = term_validator:validate([], Format),
    {invalid, not_map} = term_validator:validate({}, Format),
    valid = term_validator:validate(#{}, Format),

    valid = term_validator:validate(#{a => true}, Format),
    valid = term_validator:validate(#{a => true, b => 42}, Format),
    valid = term_validator:validate(#{a => true, b => 42, c => "Hello world!"}, Format),

    ok.

map_dynamic_validator_key_test() ->
    Format = {map_dynamic, [{key, any}]},
    Term = #{
        true => a,
        42 => b,
        "Hello world!" => c
    },
    valid = term_validator:validate(Term, Format),

    {invalid, {keys, InvalidKeys1}} =
        term_validator:validate(Term, {map_dynamic, [{key, bool}]}),
    2 = length(InvalidKeys1),
    true = lists:member({42, not_bool}, InvalidKeys1),
    true = lists:member({"Hello world!", not_bool}, InvalidKeys1),

    {invalid, {keys, InvalidKeys2}} =
        term_validator:validate(Term, {map_dynamic, [{key, number}]}),
    2 = length(InvalidKeys2),
    true = lists:member({true, not_number}, InvalidKeys2),
    true = lists:member({"Hello world!", not_number}, InvalidKeys2),

    {invalid, {keys, InvalidKeys3}} =
        term_validator:validate(Term, {map_dynamic, [{key, string}]}),
    2 = length(InvalidKeys3),
    true = lists:member({true, not_string}, InvalidKeys3),
    true = lists:member({42, not_string}, InvalidKeys3),

    KeyFormat = {any_of, [bool, number, string]},
    valid = term_validator:validate(Term, {map_dynamic, [{key, KeyFormat}]}),

    ok.

map_dynamic_validator_value_test() ->
    Format = {map_dynamic, [{value, any}]},
    Term = #{
        a => true,
        b => 42,
        c => "Hello world!"
    },
    valid = term_validator:validate(Term, Format),

    {invalid, {values, InvalidValues1}} =
        term_validator:validate(Term, {map_dynamic, [{value, bool}]}),
    2 = length(InvalidValues1),
    true = lists:member({b, not_bool}, InvalidValues1),
    true = lists:member({c, not_bool}, InvalidValues1),

    {invalid, {values, InvalidValues2}} =
        term_validator:validate(Term, {map_dynamic, [{value, number}]}),
    2 = length(InvalidValues2),
    true = lists:member({a, not_number}, InvalidValues2),
    true = lists:member({c, not_number}, InvalidValues2),

    {invalid, {values, InvalidValues3}} =
        term_validator:validate(Term, {map_dynamic, [{value, string}]}),
    2 = length(InvalidValues3),
    true = lists:member({a, not_string}, InvalidValues3),
    true = lists:member({b, not_string}, InvalidValues3),

    ValueFormat = {any_of, [bool, number, string]},
    valid = term_validator:validate(Term, {map_dynamic, [{value, ValueFormat}]}),

    ok.

map_dynamic_validator_length_test() ->
    Format1 = {map_dynamic, [{length, {2, 4}}]},
    Format2 = {map_dynamic, [{length, {{2, inclusive}, 4}}]},
    Format3 = {map_dynamic, [{length, {{2, exclusive}, 4}}]},
    Format4 = {map_dynamic, [{length, {2, {4, inclusive}}}]},
    Format5 = {map_dynamic, [{length, {2, {4, exclusive}}}]},

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} =
        term_validator:validate(#{a => 1}, Format1),
    valid = term_validator:validate(#{a => 1, b => 2}, Format1),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format1),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format1),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4, e => 5}, Format1),

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} =
        term_validator:validate(#{a => 1}, Format2),
    valid = term_validator:validate(#{a => 1, b => 2}, Format2),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format2),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format2),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4, e => 5}, Format2),

    {invalid, {length, {must_be_strictly_greater_than, 2}}} =
        term_validator:validate(#{a => 1}, Format3),
    {invalid, {length, {must_be_strictly_greater_than, 2}}} =
        term_validator:validate(#{a => 1, b => 2}, Format3),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format3),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format3),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4, e => 5}, Format3),

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} =
        term_validator:validate(#{a => 1}, Format4),
    valid = term_validator:validate(#{a => 1, b => 2}, Format4),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format4),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format4),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4, e => 5}, Format4),

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} =
        term_validator:validate(#{a => 1}, Format5),
    valid = term_validator:validate(#{a => 1, b => 2}, Format5),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format5),
    {invalid, {length, {must_be_strictly_less_than, 4}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format5),
    {invalid, {length, {must_be_strictly_less_than, 4}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4, e => 5}, Format5),

    ok.

map_dynamic_validator_length_min_test() ->
    Format1 = {map_dynamic, [{length, {3, no_max}}]},
    Format2 = {map_dynamic, [{length, {{3, inclusive}, no_max}}]},
    Format3 = {map_dynamic, [{length, {{3, exclusive}, no_max}}]},

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} =
        term_validator:validate(#{a => 1, b => 2}, Format1),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format1),

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} =
        term_validator:validate(#{a => 1, b => 2}, Format2),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format2),

    {invalid, {length, {must_be_strictly_greater_than, 3}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3}, Format3),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format3),

    ok.

map_dynamic_validator_length_max_test() ->
    Format1 = {map_dynamic, [{length, {no_min, 3}}]},
    Format2 = {map_dynamic, [{length, {no_min, {3, inclusive}}}]},
    Format3 = {map_dynamic, [{length, {no_min, {3, exclusive}}}]},

    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format1),
    {invalid, {length, {must_be_less_or_equal_to, 3}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format1),

    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format2),
    {invalid, {length, {must_be_less_or_equal_to, 3}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format2),

    valid = term_validator:validate(#{a => 1, b => 2}, Format3),
    {invalid, {length, {must_be_strictly_less_than, 3}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3}, Format3),

    ok.

map_dynamic_validator_min_test() ->
    Format1 = {map_dynamic, [{min, 3}]},
    Format2 = {map_dynamic, [{min, {3, inclusive}}]},
    Format3 = {map_dynamic, [{min, {3, exclusive}}]},

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} =
        term_validator:validate(#{a => 1, b => 2}, Format1),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format1),

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} =
        term_validator:validate(#{a => 1, b => 2}, Format2),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format2),

    {invalid, {length, {must_be_strictly_greater_than, 3}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3}, Format3),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format3),

    % Test if the 'min' option becomes an invalid when used with the 'length'
    % option.
    Format4 = {map_dynamic, [{length, {3, no_max}}, {min, 3}]},
    {invalid_options, [min]} =
        term_validator:validate(#{a => 1, b => 2, c => 3}, Format4),

    ok.

map_dynamic_validator_max_test() ->
    Format1 = {map_dynamic, [{max, 3}]},
    Format2 = {map_dynamic, [{max, {3, inclusive}}]},
    Format3 = {map_dynamic, [{max, {3, exclusive}}]},

    {invalid, {length, {must_be_less_or_equal_to, 3}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format1),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format1),

    {invalid, {length, {must_be_less_or_equal_to, 3}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3, d => 4}, Format2),
    valid = term_validator:validate(#{a => 1, b => 2, c => 3}, Format2),

    {invalid, {length, {must_be_strictly_less_than, 3}}} =
        term_validator:validate(#{a => 1, b => 2, c => 3}, Format3),
    valid = term_validator:validate(#{a => 1, b => 2}, Format3),

    % Test if the 'max' option becomes an invalid when used with the 'length'
    % option.
    Format4 = {map_dynamic, [{length, {no_min, 3}}, {max, 3}]},
    {invalid_options, [max]} =
        term_validator:validate(#{a => 1, b => 2, c => 3}, Format4),

    ok.
