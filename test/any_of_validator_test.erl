%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(any_of_validator_test).
-include_lib("eunit/include/eunit.hrl").

any_of_validator_test() ->
    AnyFormats = [
        bool,
        {number, [integer_only]},
        {string, [ascii, {pattern, "^Hello"}]}
    ],
    Format = {any_of, AnyFormats},

    {invalid, {not_any_of, AnyFormats}} = term_validator:validate(yolo, Format),
    valid = term_validator:validate(true, Format),
    valid = term_validator:validate(false, Format),
    valid = term_validator:validate(42, Format),
    {invalid, {not_any_of, AnyFormats}} = term_validator:validate(42.5, Format),
    valid = term_validator:validate("Hello world!", Format),
    {invalid, {not_any_of, AnyFormats}} = term_validator:validate("Hi world!", Format),
    {invalid, {not_any_of, AnyFormats}} = term_validator:validate([], Format),
    {invalid, {not_any_of, AnyFormats}} = term_validator:validate({}, Format),
    {invalid, {not_any_of, AnyFormats}} = term_validator:validate(#{}, Format),

    InvalidFormat = {any_of, [xyz, {abc, []}]},
    {invalid_options, [xyz, abc]} = term_validator:validate(yolo, InvalidFormat),

    ok.
