%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the Erlang Term Validator project which is
%% released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(term_validator).

-export_type([validator_name/0]).

-export_type([option_name/0]).
-export_type([option_value/0]).
-export_type([option/0]).

-export_type([options/0]).
-export_type([format/0]).
-export_type([validators/0]).

-export([validate/2, validate/3]).
-export([validators/0]).

%%
%% Main API.
%%
%% To be written.
%%
-type validator_name() :: atom().

-type option_name() :: atom().
-type option_value() :: term().
-type option() :: {atom(), term()}.

-type options() :: [option()].
-type format() :: validator_name() | {validator_name(), options()}.
-type validators() :: #{
    validator_name() := module()
}.

-callback options(mandatory | optional) -> dynamic | [option_name()].

-callback pre_validate(term(), options(), validators()) ->
    {valid, term(), options()} |
    {invalid, Reason :: term()} |
    {missing_options, [option_name()]} |
    {invalid_options, [option_name()]}
.
-callback validate(term(), option(), validators()) ->
    {valid, term()} |
    {invalid, Reason :: term()} |
    {invalid_option_value, Reason :: term()}
.
-callback post_validate(term(), validators()) ->
    valid |
    {invalid, Reason :: term()}
.

-type validate_ret() ::
    valid |
    {invalid, Reason :: term()} |
    {no_validator, validator_name()} |
    {missing_options, [option_name()]} |
    {invalid_options, [option_name()]} |
    {invalid_option_value, option_name(), Reason :: term()}
.

%%
%% @doc Brief description.
%%
%% Long description.
%%
-spec validate(term(), format()) -> validate_ret().
validate(Term, Format) ->
    validate(Term, Format, validators()).

%%
%% @doc Brief description.
%%
%% Long description.
%%
-spec validate(term(), format(), validators()) -> validate_ret().
validate(Term, Format, Validators) ->
    {Name, Options} = case Format of
        {Name_, Options_} ->
            {Name_, Options_};
        Name_ ->
            {Name_, []}
    end,

    case maps:get(Name, Validators, undefined) of
        undefined ->
            {no_validator, Name};
        Validator ->
            MandatoryOptions = Validator:options(mandatory),
            OptionalOptions = Validator:options(optional),
            case has_dynamic_options(MandatoryOptions, OptionalOptions) of
                yes ->
                    % If the validator has dynamic options, it becomes
                    % responsible for validating the options itself.
                    validate_term(Validator, Term, Options, Validators);
                no ->
                    AllOptions = MandatoryOptions ++ OptionalOptions,
                    case has_missing_options(Options, MandatoryOptions) of
                        {yes, MissingOptions} ->
                            {missing_options, MissingOptions};
                        no ->
                            case has_invalid_options(Options, AllOptions) of
                                {yes, InvalidOptions} ->
                                    {invalid_options, InvalidOptions};
                                no ->
                                    validate_term(Validator, Term, Options, Validators)
                            end
                    end
            end
    end.

%%
%% @doc Brief description.
%%
%% Long description.
%%
validators() ->
    #{
        any => any_validator,
        atom => atom_validator,
        bool => bool_validator,
        number => number_validator,
        string => string_validator,
        list => list_validator,
        tuple => tuple_validator,
        map => map_validator,
        any_of => any_of_validator,
        all_of => all_of_validator
    }.

-spec has_dynamic_options(options(), options()) -> yes | no.
has_dynamic_options(_MandatoryOptions, dynamic) ->
    yes;
has_dynamic_options(dynamic, _OptionalOptions) ->
    yes;
has_dynamic_options(_MandatoryOptions, _OptionalOptions) ->
    no.

-spec has_missing_options(options(), [option_name()]) ->
    no | {yes, [option_name()]}.
has_missing_options(Options, MandatoryOptions) ->
    % We remove mandatory option one by one if they're present. If there's
    % remaining mandatory options, then we have missing options.
    RemainingOptions = lists:dropwhile(fun(Name) ->
        proplists:is_defined(Name, Options)
    end, MandatoryOptions),
    case RemainingOptions of
        [] -> no;
        _ -> {yes, RemainingOptions}
    end.

-spec has_invalid_options(options(), [option_name()]) ->
    no | {yes, [option_name()]}.
has_invalid_options(Options, ValidatorOptions) ->
    % We check validity of options one by one and add them to a list.
    InvalidOptions = lists:foldr(fun(Option, InvalidOptions) ->
        Name = case Option of
            {Name_, _Value} -> Name_;
            Name_ -> Name_
        end,
        case lists:member(Name, ValidatorOptions) of
            true -> InvalidOptions;
            false -> [Name | InvalidOptions]
        end
    end, [], Options),
    case InvalidOptions of
        [] -> no;
        _ -> {yes, InvalidOptions}
    end.

-spec validate_term(any(), term(), options(), validators()) ->
    valid | {invalid | Reason :: term()}.
validate_term(Validator, Term, Options, Validators) ->
    case Validator:pre_validate(Term, Options, Validators) of
        {valid, NextTerm, NextOptions} ->
            case validate_term_with_options(Validator, NextTerm, NextOptions, Validators) of
                {valid, NextNextTerm} ->
                    Validator:post_validate(NextNextTerm, Validators);
                {invalid, Reason} ->
                    {invalid, Reason};
                {invalid_option_value, Option, Reason} ->
                    {invalid_option_value, Option, Reason}
            end;
        {invalid, Reason} ->
            {invalid, Reason};
        {missing_options, MissingOptions} ->
            {missing_options, MissingOptions};
        {invalid_options, InvalidOptions} ->
            {invalid_options, InvalidOptions}
    end.

validate_term_with_options(_Validator, Term, [], _Validators) ->
    {valid, Term};
validate_term_with_options(Validator, Term, [Option|Options], Validators) ->
    case Validator:validate(Term, Option, Validators) of
        {valid, NextTerm} ->
            validate_term_with_options(Validator, NextTerm, Options, Validators);
        {invalid, Reason} ->
            {invalid, Reason};
        {invalid_option_value, Reason} ->
            {invalid_option_value, Option, Reason}
    end.
