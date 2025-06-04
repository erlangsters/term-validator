%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(string_validator).
-moduledoc false.

-behaviour(term_validator).

-export([options/1]).
-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

options(mandatory) ->
    [];
options(optional) ->
    [length, min, max, alphabet, pattern, ascii, latin1].

pre_validate(Term, Options, _Validators) when is_list(Term) ->
    % We want to invalidate the 'min' and 'max' options when the 'length'
    % option is used (as they are shortcuts for the 'length' option).
    InvalidOptions = length_option_validator:invalid_options(Options),
    case InvalidOptions of
        [] ->
            {valid, Term, Options};
        _ ->
            {invalid_options, InvalidOptions}
    end;
pre_validate(_Term, _Options, _Validators) ->
    {invalid, not_string}.

validate(Term, {length, {Minimum, Maximum}}, _Validators) ->
    case length_option_validator:validate_length(length(Term), Minimum, Maximum) of
        valid ->
            {valid, Term};
        {invalid, Reason} ->
            {invalid, {length, Reason}}
    end;
validate(Term, {min, Minimum}, Validators) ->
    % The 'min' option is a shortcut for "{length, {Minimum, no_max}}".
    validate(Term, {length, {Minimum, no_max}}, Validators);
validate(Term, {max, Maximum}, Validators) ->
    % The 'max' option is a shortcut for "{length, {no_min, Maximum}}".
    validate(Term, {length, {no_min, Maximum}}, Validators);

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
