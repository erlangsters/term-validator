%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(number_validator_test).
-include_lib("eunit/include/eunit.hrl").

number_validator_test() ->
    valid = term_validator:validate(42, number),
    valid = term_validator:validate(42.5, number),
    valid = term_validator:validate(42, {number, []}),

    Format = {number, []},
    {invalid, not_number} = term_validator:validate(false, Format),
    {invalid, not_number} = term_validator:validate(true, Format),
    {invalid, not_number} = term_validator:validate("Hello world!", Format),
    {invalid, not_number} = term_validator:validate([], Format),
    {invalid, not_number} = term_validator:validate({}, Format),
    {invalid, not_number} = term_validator:validate(#{}, Format),

    ok.

number_validator_min_test() ->
    Format1 = {number, [{min, 42}]},
    Format2 = {number, [{min, {42, inclusive}}]},
    Format3 = {number, [{min, {42, exclusive}}]},

    {invalid, {must_be_greater_or_equal_to, 42}} = term_validator:validate(41, Format1),
    valid= term_validator:validate(42, Format1),
    valid = term_validator:validate(43, Format1),

    {invalid, {must_be_greater_or_equal_to, 42}} = term_validator:validate(41, Format2),
    valid= term_validator:validate(42, Format2),
    valid = term_validator:validate(43, Format2),

    {invalid, {must_be_strictly_greater_than, 42}} = term_validator:validate(41, Format3),
    {invalid, {must_be_strictly_greater_than, 42}}= term_validator:validate(42, Format3),
    valid = term_validator:validate(43, Format3),

    ok.

number_validator_max_test() ->
    Format1 = {number, [{max, 42}]},
    Format2 = {number, [{max, {42, inclusive}}]},
    Format3 = {number, [{max, {42, exclusive}}]},

    valid = term_validator:validate(41, Format1),
    valid = term_validator:validate(42, Format1),
    {invalid, {must_be_lower_or_equal_to, 42}} = term_validator:validate(43, Format1),

    valid = term_validator:validate(41, Format2),
    valid = term_validator:validate(42, Format2),
    {invalid, {must_be_lower_or_equal_to, 42}} = term_validator:validate(43, Format2),

    valid = term_validator:validate(41, Format3),
    {invalid, {must_be_strictly_lower_than, 42}} = term_validator:validate(42, Format3),
    {invalid, {must_be_strictly_lower_than, 42}} = term_validator:validate(43, Format3),

    ok.

number_validator_integer_only_test() ->
    Format = {number, [integer_only]},

    valid = term_validator:validate(42, Format),
    {invalid, must_be_integer} = term_validator:validate(42.5, Format),

    ok.

number_validator_multiple_of_test() ->
    Format = {number, [{multiple_of, 3}]},

    {invalid, {must_be_multiple_of, 3}} = term_validator:validate(41, Format),
    valid = term_validator:validate(42, Format),
    {invalid, {must_be_multiple_of, 3}} = term_validator:validate(43, Format),

    ok.
