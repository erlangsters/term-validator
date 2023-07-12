%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the Erlang Term Validator project which is
%% released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(number_validator).
-behaviour(term_validator).

-export([options/1]).
-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

options(mandatory) ->
    [];
options(optional) ->
    [min, max, integer_only, multiple_of].

pre_validate(Term, Options, _Validators) when is_number(Term) ->
    {valid, Term, Options};
pre_validate(_Term, _Options, _Validators) ->
    {invalid, not_number}.

validate(Term, {min, {Min, inclusive}}, _Validators) ->
    case Term >= Min of
        true ->
            {valid, Term};
        false ->
            {invalid, {must_be_greater_or_equal_to, Min}}
    end;
validate(Term, {min, {Min, exclusive}}, _Validators) ->
    case Term > Min of
        true ->
            {valid, Term};
        false ->
            {invalid, {must_be_strictly_greater_than, Min}}
    end;
validate(Term, {min, Min}, _Validators) ->
    validate(Term, {min, {Min, inclusive}}, _Validators);

validate(Term, {max, {Max, inclusive}}, _Validators) ->
    case Term =< Max of
        true ->
            {valid, Term};
        false ->
            {invalid, {must_be_lower_or_equal_to, Max}}
    end;
validate(Term, {max, {Max, exclusive}}, _Validators) ->
    case Term < Max of
        true ->
            {valid, Term};
        false ->
            {invalid, {must_be_strictly_lower_than, Max}}
    end;
validate(Term, {max, Max}, _Validators) ->
    validate(Term, {max, {Max, inclusive}}, _Validators);

validate(Term, integer_only, _Validators) when is_float(Term) ->
    {invalid, must_be_integer};

validate(Term, {multiple_of, Value}, _Validators) when not is_integer(Term) ->
    % The 'multiple_of' implies integers only; if it's not an integer, it's not
    % a multiple of anything.
    {invalid, {must_be_multiple_of, Value}};
validate(Term, {multiple_of, Value}, _Validators) ->
    case Term rem Value of
        0 ->
            {valid, Term};
        _ ->
            {invalid, {must_be_multiple_of, Value}}
    end;

validate(Term, _Option, _Validators) ->
    {valid, Term}.

post_validate(_Term, _Validators) ->
    valid.
