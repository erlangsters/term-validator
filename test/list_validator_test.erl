%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the Erlang Term Validator project which is
%% released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(list_validator_test).
-include_lib("eunit/include/eunit.hrl").

list_validator_test() ->
    valid = term_validator:validate([], {list, [{item, any}]}),
    {missing_options, [item]} = term_validator:validate([], list),
    {missing_options, [item]} = term_validator:validate([], {list, []}),

    Format = {list, [{item, any}]},
    {invalid, not_list} = term_validator:validate(yolo, Format),
    {invalid, not_list} = term_validator:validate(42, Format),
    valid = term_validator:validate("Hello world!", Format),
    {invalid, not_list} = term_validator:validate({}, Format),
    {invalid, not_list} = term_validator:validate(#{}, Format),

     ok.

list_validator_item_test() ->
    Format = {list, [{item, any}]},
    Term = [true, 42, "Hello world!"],
    valid = term_validator:validate(Term, Format),

    {invalid, {items, [{2, not_bool}, {3, not_bool}]}} =
        term_validator:validate(Term, {list, [{item, bool}]}),

    {invalid, {items, [{1, not_number}, {3, not_number}]}} =
        term_validator:validate(Term, {list, [{item, number}]}),

    {invalid, {items, [{1, not_string}, {2, not_string}]}} =
        term_validator:validate(Term, {list, [{item, string}]}),

    ItemFormat = {any_of, [bool, number, string]},
    valid = term_validator:validate(Term, {list, [{item, ItemFormat}]}),

    ok.

list_validator_length_test() ->
    Format1 = {list, [{item, any}, {length, {2, 4}}]},
    Format2 = {list, [{item, any}, {length, {{2, inclusive}, 4}}]},
    Format3 = {list, [{item, any}, {length, {{2, exclusive}, 4}}]},
    Format4 = {list, [{item, any}, {length, {2, {4, inclusive}}}]},
    Format5 = {list, [{item, any}, {length, {2, {4, exclusive}}}]},

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} = term_validator:validate([1], Format1),
    valid = term_validator:validate([1, 2], Format1),
    valid = term_validator:validate([1, 2, 3], Format1),
    valid = term_validator:validate([1, 2, 3, 4], Format1),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} = term_validator:validate([1, 2, 3, 4, 5], Format1),

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} = term_validator:validate([1], Format2),
    valid = term_validator:validate([1, 2], Format2),
    valid = term_validator:validate([1, 2, 3], Format2),
    valid = term_validator:validate([1, 2, 3, 4], Format2),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} = term_validator:validate([1, 2, 3, 4, 5], Format2),

    {invalid, {length, {must_be_strictly_greater_than, 2}}} = term_validator:validate([1], Format3),
    {invalid, {length, {must_be_strictly_greater_than, 2}}} = term_validator:validate([1, 2], Format3),
    valid = term_validator:validate([1, 2, 3], Format3),
    valid = term_validator:validate([1, 2, 3, 4], Format3),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} = term_validator:validate([1, 2, 3, 4, 5], Format3),

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} = term_validator:validate([1], Format4),
    valid = term_validator:validate([1, 2], Format4),
    valid = term_validator:validate([1, 2, 3], Format4),
    valid = term_validator:validate([1, 2, 3, 4], Format4),
    {invalid, {length, {must_be_less_or_equal_to, 4}}} = term_validator:validate([1, 2, 3, 4, 5], Format4),

    {invalid, {length, {must_be_greater_or_equal_to, 2}}} = term_validator:validate([1], Format5),
    valid = term_validator:validate([1, 2], Format5),
    valid = term_validator:validate([1, 2, 3], Format5),
    {invalid, {length, {must_be_strictly_less_than, 4}}} = term_validator:validate([1, 2, 3, 4], Format5),
    {invalid, {length, {must_be_strictly_less_than, 4}}} = term_validator:validate([1, 2, 3, 4, 5], Format5),

    ok.

list_validator_length_min_test() ->
    Format1 = {list, [{item, any}, {length, {3, no_max}}]},
    Format2 = {list, [{item, any}, {length, {{3, inclusive}, no_max}}]},
    Format3 = {list, [{item, any}, {length, {{3, exclusive}, no_max}}]},

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} = term_validator:validate([1, 2], Format1),
    valid = term_validator:validate([1, 2, 3], Format1),

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} = term_validator:validate([1, 2], Format2),
    valid = term_validator:validate([1, 2, 3], Format2),

    {invalid, {length, {must_be_strictly_greater_than, 3}}} = term_validator:validate([1, 2, 3], Format3),
    valid = term_validator:validate([1, 2, 3, 4], Format3),

    ok.

list_validator_length_max_test() ->
    Format1 = {list, [{item, any}, {length, {no_min, 3}}]},
    Format2 = {list, [{item, any}, {length, {no_min, {3, inclusive}}}]},
    Format3 = {list, [{item, any}, {length, {no_min, {3, exclusive}}}]},

    valid = term_validator:validate([1, 2, 3], Format1),
    {invalid, {length, {must_be_less_or_equal_to, 3}}} = term_validator:validate([1, 2, 3, 4], Format1),

    valid = term_validator:validate([1, 2, 3], Format2),
    {invalid, {length, {must_be_less_or_equal_to, 3}}} = term_validator:validate([1, 2, 3, 4], Format2),

    valid = term_validator:validate([1, 2], Format3),
    {invalid, {length, {must_be_strictly_less_than, 3}}} = term_validator:validate([1, 2, 3], Format3),

    ok.

list_validator_min_test() ->
    Format1 = {list, [{item, any}, {min, 3}]},
    Format2 = {list, [{item, any}, {min, {3, inclusive}}]},
    Format3 = {list, [{item, any}, {min, {3, exclusive}}]},

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} =
        term_validator:validate([1, 2], Format1),
    valid = term_validator:validate([1, 2, 3], Format1),

    {invalid, {length, {must_be_greater_or_equal_to, 3}}} =
        term_validator:validate([1, 2], Format2),
    valid = term_validator:validate([1, 2, 3], Format2),

    {invalid, {length, {must_be_strictly_greater_than, 3}}} =
        term_validator:validate([1, 2, 3], Format3),
    valid = term_validator:validate([1, 2, 3, 4], Format3),

    % Test if the 'min' option becomes an invalid when used with the 'length'
    % option.
    Format4 = {list, [{item, any}, {length, {3, no_max}}, {min, 3}]},
    {invalid_options, [min]} = term_validator:validate([1, 2, 3], Format4),

    ok.

list_validator_max_test() ->
    Format1 = {list, [{item, any}, {max, 3}]},
    Format2 = {list, [{item, any}, {max, {3, inclusive}}]},
    Format3 = {list, [{item, any}, {max, {3, exclusive}}]},

    {invalid, {length, {must_be_less_or_equal_to, 3}}} =
        term_validator:validate([1, 2, 3, 4], Format1),
    valid = term_validator:validate([1, 2, 3], Format1),

    {invalid, {length, {must_be_less_or_equal_to, 3}}} =
        term_validator:validate([1, 2, 3, 4], Format2),
    valid = term_validator:validate([1, 2, 3], Format2),

    {invalid, {length, {must_be_strictly_less_than, 3}}} =
        term_validator:validate([1, 2, 3], Format3),
    valid = term_validator:validate([1, 2], Format3),

    % Test if the 'max' option becomes an invalid when used with the 'length'
    % option.
    Format4 = {list, [{item, any}, {length, {no_min, 3}}, {max, 3}]},
    {invalid_options, [max]} = term_validator:validate([1, 2, 3], Format4),

    ok.
