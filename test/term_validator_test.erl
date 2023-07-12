%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the Erlang Term Validator project which is
%% released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, March 2023
%%
-module(term_validator_test).

-include_lib("eunit/include/eunit.hrl").

term_validator_test() ->
    % XXX: Complete this after implementing the built-in validators.

    % valid = term_validator:validate(true, bool),
    % valid = term_validator:validate(false, bool),
    % valid = term_validator:validate(42, integer),
    % valid = term_validator:validate(42.5, float),
    % valid = term_validator:validate(42, number),
    % valid = term_validator:validate(42.5, number),
    % valid = term_validator:validate("Hello world!", string),
    % valid = term_validator:validate([], list),
    % valid = term_validator:validate({}, tuple),
    % valid = term_validator:validate(#{}, map),

    ok.

term_validator_no_validator_test() ->
    % test with a non built-in validator
    {no_validator, foo} = term_validator:validate(foo, foo),

    % test a built-in validator after overriding the available validators
    {no_validator, number} = term_validator:validate(42, number, #{}),

    ok.

term_validator_missing_options_test() ->
    meck:new(yolo_validator, [non_strict]),
    meck:expect(yolo_validator, options, fun
        (mandatory) -> [foo];
        (optional) -> [bar, quz]
    end),
    meck:expect(yolo_validator, pre_validate, fun(Term, Options, _) -> {valid, Term, Options} end),
    meck:expect(yolo_validator, validate, fun(Term, _, _) -> {valid, Term} end),
    meck:expect(yolo_validator, post_validate, fun(_, _) -> valid end),

    Validators = #{yolo => yolo_validator},
    {missing_options, [foo]} = term_validator:validate(42, yolo, Validators),

    valid = term_validator:validate(42, {yolo, [{foo, 42}]}, Validators),
    {missing_options, [foo]} = term_validator:validate(42, {yolo, [{bar, 42}]}, Validators),
    {missing_options, [foo]} = term_validator:validate(42, {yolo, [{quz, 42}]}, Validators),

    meck:unload(yolo_validator),

    ok.

term_validator_invalid_options_test() ->
    meck:new(yolo_validator, [non_strict]),
    meck:expect(yolo_validator, options, fun
        (mandatory) -> [];
        (optional) -> [foo]
    end),
    meck:expect(yolo_validator, pre_validate, fun(Term, Options, _) -> {valid, Term, Options} end),
    meck:expect(yolo_validator, validate, fun(Term, _, _) -> {valid, Term} end),
    meck:expect(yolo_validator, post_validate, fun(_, _) -> valid end),

    Validators = #{yolo => yolo_validator},
    valid = term_validator:validate(42, yolo, Validators),
    valid = term_validator:validate(42, {yolo, [{foo, 42}]}, Validators),
    {invalid_options, [bar]} = term_validator:validate(42, {yolo, [{foo, 42}, {bar, 42}]}, Validators),
    {invalid_options, [bar, quz]} = term_validator:validate(42, {yolo, [{foo, 42}, {bar, 42}, {quz, 42}]}, Validators),

    meck:unload(yolo_validator),
    ok.

term_validator_dynamic_mandatory_options_test() ->
    meck:new(yolo_validator, [non_strict]),
    meck:expect(yolo_validator, options, fun
        (mandatory) -> dynamic;
        (optional) -> []
    end),
    meck:expect(yolo_validator, pre_validate, fun(Term, Options, _) -> {valid, Term, Options} end),
    meck:expect(yolo_validator, validate, fun(Term, _, _) -> {valid, Term} end),
    meck:expect(yolo_validator, post_validate, fun(_, _) -> valid end),

    Validators = #{yolo => yolo_validator},
    valid = term_validator:validate(42, {yolo, [{foo, 42}]}, Validators),

    meck:expect(yolo_validator, pre_validate, fun(_, _, _) ->
        {missing_options, [foo]}
    end),
    {missing_options, [foo]} = term_validator:validate(42, yolo, Validators),

    meck:expect(yolo_validator, pre_validate, fun(_, _, _) ->
        {invalid_options, [foo]}
    end),
    {invalid_options, [foo]} = term_validator:validate(42, {yolo, [{bar, 42}]}, Validators),

    meck:unload(yolo_validator),

    ok.

term_validator_dynamic_optional_options_test() ->
    meck:new(yolo_validator, [non_strict]),
    meck:expect(yolo_validator, options, fun
        (mandatory) -> [];
        (optional) -> dynamic
    end),
    meck:expect(yolo_validator, pre_validate, fun(Term, Options, _) -> {valid, Term, Options} end),
    meck:expect(yolo_validator, validate, fun(Term, _, _) -> {valid, Term} end),
    meck:expect(yolo_validator, post_validate, fun(_, _) -> valid end),

    Validators = #{yolo => yolo_validator},
    valid = term_validator:validate(42, {yolo, [{foo, 42}]}, Validators),

    meck:expect(yolo_validator, pre_validate, fun(_, _, _) ->
        {missing_options, [foo]}
    end),
    {missing_options, [foo]} = term_validator:validate(42, yolo, Validators),

    meck:expect(yolo_validator, pre_validate, fun(_, _, _) ->
        {invalid_options, [foo]}
    end),
    {invalid_options, [foo]} = term_validator:validate(42, {yolo, [{bar, 42}]}, Validators),

    meck:unload(yolo_validator),

    ok.

term_validator_pre_validate_test() ->
    Options = [{foo, true}, {bar, 42}, {quz, "Hello world!"}],

    meck:new(yolo_validator, [non_strict]),
    meck:expect(yolo_validator, options, fun
        (mandatory) -> [];
        (optional) -> [foo, bar, quz]
    end),
    meck:expect(yolo_validator, pre_validate, fun(42, Options, _) -> {valid, 43, Options} end),
    meck:expect(yolo_validator, validate, fun(43, _, _) -> {valid, 43} end),
    meck:expect(yolo_validator, post_validate, fun(_, _) -> valid end),

    Validators = #{yolo => yolo_validator},
    valid = term_validator:validate(42, {yolo, Options}, Validators),

    meck:expect(yolo_validator, pre_validate, fun(42, Options, _) -> {invalid, oloy} end),
    {invalid, oloy} = term_validator:validate(42, {yolo, Options}, Validators),

    meck:unload(yolo_validator),

    ok.

term_validator_pre_validate_options_test() ->
    % Variation of the previous test that transform the options.
    Options = [{foo, true}, {bar, 42}, {quz, "Hello world!"}],

    meck:new(yolo_validator, [non_strict]),
    meck:expect(yolo_validator, options, fun
        (mandatory) -> [];
        (optional) -> [foo, bar, quz]
    end),
    meck:expect(
        yolo_validator,
        pre_validate,
        fun(1, [{foo, true}, {bar, 42}, {quz, "Hello world!"}], _) ->
            {valid, 2, [{quz, true}, {bar, 42}, {foo, "Hello world!"}]}
        end
    ),
    meck:expect(yolo_validator, validate, fun
        (2, {quz, true}, _) -> {valid, 3};
        (3, {bar, 42}, _) -> {valid, 4};
        (4, {foo, "Hello world!"}, _) -> {valid, 5}
    end),
    meck:expect(yolo_validator, post_validate, fun(5, _) -> valid end),

    Validators = #{yolo => yolo_validator},
    valid = term_validator:validate(1, {yolo, Options}, Validators),

    meck:unload(yolo_validator),

    ok.

term_validator_validate_test() ->
    Options = [{foo, true}, {bar, 42}, {quz, "Hello world!"}],

    meck:new(yolo_validator, [non_strict]),
    meck:expect(yolo_validator, options, fun
        (mandatory) -> [];
        (optional) -> [foo, bar, quz]
    end),
    meck:expect(yolo_validator, pre_validate, fun(Term, Options, _) -> {valid, Term, Options} end),
    meck:expect(yolo_validator, validate, fun
        (42, {foo, true}, _) -> {valid, 43};
        (43, {bar, 42}, _) -> {valid, 44};
        (44, {quz, "Hello world!"}, _) -> {valid, 45}
    end),
    meck:expect(yolo_validator, post_validate, fun(45, _) -> valid end),

    Validators = #{yolo => yolo_validator},
    valid = term_validator:validate(42, {yolo, Options}, Validators),

    meck:expect(yolo_validator, validate, fun
        (42, {foo, true}, _) -> {valid, 43};
        (43, {bar, 42}, _) -> {invalid, oloy};
        (44, {quz, "Hello world!"}, _) -> {valid, 45}
    end),
    {invalid, oloy} = term_validator:validate(42, {yolo, Options}, Validators),

    meck:expect(yolo_validator, validate, fun
        (42, {foo, true}, _) -> {valid, 43};
        (43, {bar, 42}, _) -> {invalid_option_value, oloy};
        (44, {quz, "Hello world!"}, _) -> {valid, 45}
    end),
    {invalid_option_value, {bar, 42}, oloy} = term_validator:validate(42, {yolo, Options}, Validators),

    meck:unload(yolo_validator),

    ok.

term_validator_post_validate_test() ->
    Options = [{foo, true}, {bar, 42}, {quz, "Hello world!"}],

    meck:new(yolo_validator, [non_strict]),
    meck:expect(yolo_validator, options, fun
        (mandatory) -> [];
        (optional) -> [foo, bar, quz]
    end),
    meck:expect(yolo_validator, pre_validate, fun(Term, Options, _) -> {valid, Term, Options} end),
    meck:expect(yolo_validator, validate, fun(Term, _, _) -> {valid, Term} end),
    meck:expect(yolo_validator, post_validate, fun(42, _) -> valid end),

    Validators = #{yolo => yolo_validator},

    valid = term_validator:validate(42, {yolo, Options}, Validators),

    meck:expect(yolo_validator, post_validate, fun(42, _) -> {invalid, oloy} end),
    {invalid, oloy} = term_validator:validate(42, {yolo, Options}, Validators),

    meck:unload(yolo_validator),

    ok.
