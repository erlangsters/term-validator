%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(length_option_validator).
-moduledoc false.

-export([invalid_options/1]).
-export([validate_length/3]).

%%
%% This module implements logic related to the length option which is common
%% to the string and list validators.
%%
-type clusivity() :: inclusive | exclusive.
-type value() :: integer() | {integer(), clusivity()}.
-type min_value() :: no_min | value().
-type max_value() :: no_max | value().

%%
%% Check for invalid options.
%%
%% This function checks if 'min' and/or 'max' options are being used with the
%% 'length' option (which are mutually exclusive and therefore invalid when
%% used together), and return the list of invalid options.
%%
-spec invalid_options(term_validator:options()) -> [min | max].
invalid_options(Options) ->
    case proplists:is_defined(length, Options) of
        true ->
            InvalidOptions1 = case proplists:is_defined(min, Options) of
                true ->
                    [min];
                false ->
                    []
            end,
            InvalidOptions2 = case proplists:is_defined(max, Options) of
                true ->
                    InvalidOptions1 ++ [max];
                false ->
                    InvalidOptions1
            end,
            InvalidOptions2;
        false ->
            []
    end.

%%
%% Validate the length option value.
%%
%% This function validates the length option value.
%%
-spec validate_length(integer(), min_value(), max_value()) ->
    valid | {invalid, {
        must_be_greater_or_equal_to |
        must_be_strictly_greater_than |
        must_be_less_or_equal_to |
        must_be_strictly_less_than,
        integer()
    }}
.
validate_length(Length, Minimum, Maximum) ->
    case validate_length_minimum(Length, normalize_length_value(Minimum)) of
        valid ->
            validate_length_maximum(Length, normalize_length_value(Maximum));
        {invalid, Reason} ->
            {invalid, Reason}
    end.

validate_length_minimum(_Length, no_min) ->
    valid;
validate_length_minimum(Length, {Value, inclusive}) ->
    case Length >= Value of
        true ->
            valid;
        false ->
            {invalid, {must_be_greater_or_equal_to, Value}}
    end;
validate_length_minimum(Length, {Value, exclusive}) ->
    case Length > Value of
        true ->
            valid;
        false ->
            {invalid, {must_be_strictly_greater_than, Value}}
    end.

validate_length_maximum(_Length, no_max) ->
    valid;
validate_length_maximum(Length, {Value, inclusive}) ->
    case Length =< Value of
        true ->
            valid;
        false ->
            {invalid, {must_be_less_or_equal_to, Value}}
    end;
validate_length_maximum(Length, {Value, exclusive}) ->
    case Length < Value of
        true ->
            valid;
        false ->
            {invalid, {must_be_strictly_less_than, Value}}
    end.

normalize_length_value({Value, Clusivity}) ->
    {Value, Clusivity};
normalize_length_value(no_min) ->
    no_min;
normalize_length_value(no_max) ->
    no_max;
normalize_length_value(Value) ->
    {Value, inclusive}.
