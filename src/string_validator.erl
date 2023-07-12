%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the Erlang Term Validator project which is
%% released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(string_validator).
-behaviour(term_validator).

-export([options/1]).
-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

options(mandatory) ->
    [];
options(optional) ->
    [length, alphabet, pattern, ascii, latin1].

pre_validate(Term, _Options, _Validators) when is_list(Term) ->
    {valid, Term};
pre_validate(_Term, _Options, _Validators) ->
    {invalid, not_string}.

validate(Term, {length, {Minimum, Maximum}}, _Validators) ->
    case validate_length(length(Term), Minimum, Maximum) of
        valid ->
            {valid, Term};
        {invalid, Reason} ->
            {invalid, Reason}
    end;
validate(Term, {alphabet, CharacterSet}, _Validators) when is_atom(CharacterSet) ->
    Max = case CharacterSet of
        ascii -> 127;
        latin1 -> 255
    end,
    Result = lists:search(
        fun({Char, _Pos}) -> Char > Max end,
        lists:zip(Term, lists:seq(1, length(Term)))
    ),
    case Result of
        {value, {Char, Position}} ->
            {invalid, {wrong_character, Char, position, Position}};
        false ->
            {valid, Term}
    end;
validate(Term, {alphabet, CharacterSet}, _Validators) ->
    Result = lists:search(
        fun({Char, _Pos}) -> not lists:member(Char, CharacterSet) end,
        lists:zip(Term, lists:seq(1, length(Term)))
    ),
    case Result of
        {value, {Char, Position}} ->
            {invalid, {wrong_character, Char, position, Position}};
        false ->
            {valid, Term}
    end;
validate(Term, ascii, _Validators) ->
    validate(Term, {alphabet, ascii}, _Validators);
validate(Term, latin1, _Validators) ->
    validate(Term, {alphabet, latin1}, _Validators);
validate(Term, {pattern, Pattern}, _Validators) ->
    {ok, Regex} = re:compile(Pattern),
    case re:run(Term, Regex) of
        {match, _} ->
            {valid, Term};
        nomatch ->
            {invalid, {pattern_mismatch, Pattern}}
    end.

post_validate(_Term, _Validators) ->
    valid.

normalize_length_value({Value, Clusivity}) ->
    {Value, Clusivity};
normalize_length_value(no_min) ->
    no_min;
normalize_length_value(no_max) ->
    no_max;
normalize_length_value(Value) ->
    {Value, inclusive}.

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
            {invalid, {too_short, must_be_greater_or_equal_to, Value}}
    end;
validate_length_minimum(Length, {Value, exclusive}) ->
    case Length > Value of
        true ->
            valid;
        false ->
            {invalid, {too_short, must_be_strictly_greater_than, Value}}
    end.

validate_length_maximum(_Length, no_max) ->
    valid;
validate_length_maximum(Length, {Value, inclusive}) ->
    case Length =< Value of
        true ->
            valid;
        false ->
            {invalid, {too_long, must_be_less_or_equal_to, Value}}
    end;
validate_length_maximum(Length, {Value, exclusive}) ->
    case Length < Value of
        true ->
            valid;
        false ->
            {invalid, {too_long, must_be_strictly_less_than, Value}}
    end.
