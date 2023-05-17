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

-export([validate/2, validate/3]).
-export([validators/0]).

%%
%% Main API.
%%
%% To be written.
%%
-type validator_name() :: atom().
-type validator_option_name() :: atom().
-type validator_option() :: {validator_option_name(), term()}.
-type validator_options() :: [validator_option()].
-type term_format() :: validator_name() | {validator_name(), validator_options()}.

-type validators() :: #{
    validator_name() := module()
}.

-callback mandatory_options() -> [validator_option_name()].
-callback options() -> [validator_option_name()].

-callback pre_validate(term(), validator_options(), validators()) ->
    {valid, term()} |
    {invalid, Reason :: term()}
.
-callback validate(term(), validator_option(), validators()) ->
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
    {missing_options, [atom()]} |
    {invalid_options, [atom()]} |
    {invalid_option_value, atom(), Reason :: term()}
.

%%
%% @doc Brief description.
%%
%% Long description.
%%
-spec validate(term(), term_format()) -> validate_ret().
validate(Term, Format) ->
    validate(Term, Format, validators()).

%%
%% @doc Brief description.
%%
%% Long description.
%%
-spec validate(term(), term_format(), validators()) -> validate_ret().
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
            ValidatorMandatoryOptions = Validator:mandatory_options(),
            ValidatorOptions = Validator:options(),
            AllValidatorOptions = ValidatorMandatoryOptions ++ ValidatorOptions,
            case has_missing_options(Options, ValidatorMandatoryOptions) of
                {yes, MissingOptions} ->
                    {missing_options, MissingOptions};
                no ->
                    case has_invalid_options(Options, AllValidatorOptions) of
                        {yes, InvalidOptions} ->
                            {invalid_options, InvalidOptions};
                        no ->
                            validate_term(Validator, Term, Options, Validators)
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
        % XXX: built-in validators to be added here...
    }.

-spec has_missing_options(validator_options(), [validator_option_name()]) ->
    no | {yes, [validator_option_name()]}.
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

-spec has_invalid_options(validator_options(), [validator_option_name()]) ->
    no | {yes, [validator_option_name()]}.
has_invalid_options(Options, ValidatorOptions) ->
    % We check validity of options one by one and add them to a list.
    InvalidOptions = lists:foldr(fun({Name, _Value}, InvalidOptions) ->
        case lists:member(Name, ValidatorOptions) of
            true -> InvalidOptions;
            false -> [Name | InvalidOptions]
        end
    end, [], Options),
    case InvalidOptions of
        [] -> no;
        _ -> {yes, InvalidOptions}
    end.

-spec validate_term(any(), term(), validator_options(), validators()) ->
    valid | {invalid | Reason :: term()}.
validate_term(Validator, Term, Options, Validators) ->
    case Validator:pre_validate(Term, Options, Validators) of
        {valid, NextTerm} ->
            case validate_term_with_options(Validator, NextTerm, Options, Validators) of
                {valid, NextNextTerm} ->
                    Validator:post_validate(NextNextTerm, Validators);
                {invalid, Reason} ->
                    {invalid, Reason};
                {invalid_option_value, Option, Reason} ->
                    {invalid_option_value, Option, Reason}
            end;
        {invalid, Reason} ->
            {invalid, Reason}
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
