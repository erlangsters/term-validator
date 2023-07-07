%%
%% Copyright (c) 2023, Byteplug LLC.
%%
%% This source file is part of the Erlang Term Validator project which is
%% released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, July 2023
%%
-module(any_validator_test).
-include_lib("eunit/include/eunit.hrl").

any_validator_test() ->
    valid = term_validator:validate(any, any),
    valid = term_validator:validate(any, {any, []}),

    Format = {any, []},
    valid = term_validator:validate(hello_world, Format),

    valid = term_validator:validate(false, Format),
    valid = term_validator:validate(42, Format),
    valid = term_validator:validate("Hello world!", Format),

    valid = term_validator:validate([true, 42, "hello world!"], Format),
    valid = term_validator:validate({foo, bar, quz}, Format),
    valid = term_validator:validate(#{
        foo => true,
        bar => 42,
        quz => "Hello world!"
    }, Format),

    ok.
