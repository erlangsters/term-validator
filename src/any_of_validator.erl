%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
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
    dynamic;
options(optional) ->
    dynamic.

pre_validate(Term, [{formats, Formats}] = Options, Validators) ->
    % We want to check if there is a validator for each format. Note that
    % instead of being reported as an invalid option value, it's reported as
    % invalid options.
    MissingValidators = lists:foldr(
        fun
            F({Name, _Value}, Accumulator) ->
                F(Name, Accumulator);
            F(Name, Accumulator) ->
                case maps:is_key(Name, Validators) of
                    true ->
                        Accumulator;
                    false ->
                        [Name|Accumulator]
                end
        end,
        [],
        Formats
    ),
    case MissingValidators of
        [] ->
            {valid, Term, Options};
        _ ->
            {invalid_options, MissingValidators}
    end;
pre_validate(Term, Options, Validators) ->
    % To allow a nicer syntax, the options are actually the list of formats
    % instead. We transform the list of formats into the 'formats' option and
    % do the actual validation when processing the 'formats' option.
    pre_validate(Term, [{formats, Options}], Validators).

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
