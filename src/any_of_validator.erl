%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the Erlang Term Validator project which is
%% released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(any_of_validator).
-behaviour(term_validator).

-export([options/1]).
-export([pre_validate/3]).
-export([validate/3]).
-export([post_validate/2]).

options(mandatory) ->
    [formats];
options(optional) ->
    [].

pre_validate(Term, _Options, _Validators) ->
    {valid, Term}.

validate(Term, {formats, Formats}, Validators) ->
    Result = lists:any(fun(Format) ->
        term_validator:validate(Term, Format, Validators) == valid
    end, Formats),
    case Result of
        true ->
            {valid, Term};
        false ->
            {invalid, {not_any_of, Formats}}
    end.

post_validate(_Term, _Validators) ->
    valid.
