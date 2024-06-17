%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(atom_validator).
-behaviour(term_validator).

-export([options/1]).
-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

options(mandatory) ->
    [];
options(optional) ->
    [one_of, allow_string].

pre_validate(Term, Options, _Validators) when is_atom(Term) ->
    {valid, Term, Options};
pre_validate(Term, Options, _Validators) when is_list(Term) ->
    case lists:member(allow_string, Options) of
        true ->
            {valid, list_to_atom(Term), Options};
        false ->
            {invalid, not_atom}
    end;
pre_validate(_Term, _Options, _Validators) ->
    {invalid, not_atom}.

validate(Term, {one_of, Items}, _Validators) ->
    case lists:member(Term, Items) of
        true ->
            {valid, Term};
        false ->
            {invalid, {not_one_of, Items}}
    end;
validate(Term, allow_string, _Validators) ->
    {valid, Term}.

post_validate(_Term, _Validators) ->
    valid.
