%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(bool_validator).
-moduledoc false.

-behaviour(term_validator).

-export([options/1]).
-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

options(mandatory) ->
    [];
options(optional) ->
    [allow_number].

pre_validate(true, Options, _Validators) ->
    {valid, true, Options};
pre_validate(false, Options, _Validators) ->
    {valid, false, Options};
pre_validate(Term, Options, _Validators) ->
    case lists:member(allow_number, Options) of
        true ->
            {valid, Term, Options};
        false ->
            {invalid, not_bool}
    end.

validate(Term, _Option, _Validators) ->
    {valid, Term}.

post_validate(_Term, _Validators) ->
    valid.
