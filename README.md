# Term Validator

[![Erlangsters Repository](https://img.shields.io/badge/erlangsters-term--validator-%23a90432)](https://github.com/erlangsters/term-validator)
![Supported Erlang/OTP Versions](https://img.shields.io/badge/erlang%2Fotp-27%7C28-%23a90432)
![Current Version](https://img.shields.io/badge/version-1.0.0-%23354052)
![License](https://img.shields.io/github/license/erlangsters/term-validator)
[![Build Status](https://img.shields.io/github/actions/workflow/status/erlangsters/term-validator/workflow.yml)](https://github.com/erlangsters/term-validator/actions/workflows/workflow.yml)
[![Documentation Link](https://img.shields.io/badge/documentation-available-yellow)](http://erlangsters.github.io/term-validator/)

A quick and simple Erlang term validator. It supports validation of the common
built-in Erlang types and can be extended with custom validators.

```erlang
Format = {number, [{min, 10}]}.

valid = term_validator:validate(42, Format).
{invalid, not_number} = term_validator:validate("Hello world!", Format).
```

Consult the documentation to know how to use the built-in validators or write
your own ones. You may also want to check the regression suites in the *test/*
folder for more examples.

> If you need to validate settings/config files or JSON data, check
> out the [Settings Validator](https://github.com/erlangsters/settings-validator)
> and [JSON Validator](https://github.com/erlangsters/json-validator) projects.

Written by the Erlangsters [community](https://about.erlangsters.org/) and
released under the MIT [license](https://opensource.org/license/mit).

## Getting started

The `term_validator` module features a single `validate/2` function that takes
an Erlang term and its expected format, then returns `valid` or
`{invalid, Reason}`.

A **term format** is a tuple with the first element being the validator name,
and the second element being a list of options to be passed to the underlying
validate functions. If no options are needed, the format can simply be the
validator name.

For instance, those two term formats are equivalent.

```erlang
valid = term_validator:validate(42, number).
valid = term_validator:validate(42, {number, []}).
```

Types such as list, tuples and maps can nest other types. This is why their
validators take other term formats as options (in order to validate the
nested types).

For instance, the following format validates any list with at least two numbers
that are all greater than or equal to 10.

```erlang
Format = {list, [
    {item, {number, [{min, 10}]}},
    {min, 2}
]}.

{invalid, not_list} =
  term_validator:validate(42, Format).
{invalid, {length, {must_be_greater_or_equal, 2}}} =
  term_validator:validate([42], Format).
{invalid, {items, [{1, not_number}]}} =
  term_validator:validate([zero, 42], Format).
{invalid, {items, [{1, {value, {must_be_greater_or_equal, 10}}}]}} =
  term_validator:validate([0, 42], Format).

valid = term_validator:validate([24, 42], Format).
```

(Notice that the `{number, [{min, 10}]}` is the format of the nested type.)

By default, the validate function uses a list of built-in validators (which
this project has already implemented for you) but you may extend (or replace)
this list with your custom validators.

Suppose you have implemented the `foo` validator in the `my_foo_validator`
module, then you can pass it to the validate function as follows.

```erlang
Format = {list, [{item, foo}]},
Validators = maps:merge(term_validator:validators(), #{foo => my_foo_validator}),
valid = term_validator:validate([foo, foo, foo], Format, Validators),

% If you do not add your custom validator, it's reported.
{no_validator, foo} = term_validator:validate([foo, foo, foo], Format),
```

With that example being shown, in practice you hardly ever need to create
custom validators as most complex validation rules be created with a
combination of the built-in validators.

Refer to the list of built-in validators to understand how to write your term
format. You will find a quick [table](#the-built-in-validators) below but
they're extensively explained in the [documentation](/DOCUMENTATION.md).

## Using it in your project

With the **Rebar3** build system, add the following to the `rebar.config` file
of your project.

```
{deps, [
  {term_validator, {git, "https://github.com/erlangsters/term-validator.git", {tag, "master"}}}
]}.
```

If you happen to use the **Erlang.mk** build system, then add the following to
your Makefile.

```
BUILD_DEPS = term_validator
dep_term_validator = git https://github.com/erlangsters/term-validator master
```

In practice, you want to replace the branch "master" with a specific "tag" to
avoid breaking your project if incompatible changes are made.

## The built-in validators

The need for validating values is often encountered when dealing with external
input that uses the basic data structures such as strings, numbers or booleans,
and sometimes nested data structures such as lists, tuples or maps. In fact,
those input are not always Erlang terms directly but instead are converted to
Erlang terms from another format such as JSON. This is why this library does
not support all built-in Erlang types but only the common ones. However, you
can easily extend it with your own validators.

Here is the list of the built-in validators with their options.

| Validator       | Options                                                                  |
| --------------- | ------------------------------------------------------------------------ |
| `any`           | *No options.*                                                            |
| `atom`          | `one_of`, `allow_string`                                                 |
| `bool`          | `allow_number`                                                           |
| `number`        | `min`, `max`, `integer_only`, `multiple_of`                              |
| `string`        | `length`, `min`\*, `max`\*, `alphabet`, `pattern`, `ascii`\*, `latin1`\* |
| `list`          | `item`, `length`, `min`\*, `max`\*                                       |
| `tuple`         | `elements`                                                               |
| `tuple_dynamic` | `element`, `length`, `min`\*, `max`\*                                    |
| `map`           | `fields`                                                                 |
| `map_dynamic`   | `key`, `value`, `length`, `min`\*, `max`\*                               |
| `any_of`        | *Any term format.* **                                                    |
| `all_of`        | *Any term format.* **                                                    |

*Shortcut options; they cannot be used with the `length` option.  
**Instead of being a list of options, it's a list of term format.

The `any` validator will validate any Erlang term (useful in some scenarios).

If you're interested in validators for more complex types such as IPs, ports,
dates, colors, etc, you may want to check out the Extended Term Validator
repository (https://github.com/erlangsters/extended-term-validator).

## Writing a new validator

A validator is a module that implements a set of validate functions and defines
a list of options it supports.

A concrete example will speak louder than words.

```erlang
-module(my_validator).

-export([options/1]).
-export([pre_validate/3, validate/3, post_validate/2]).

options(mandatory) -> [foo];
options(optional) -> [bar, quz].

pre_validate(Term, Options, _Validators) -> {valid, Term, Options}.

validate(Term, {foo, _Value}, _Validators) -> {valid, Term}.
validate(Term, {bar, _Value}, _Validators) -> {valid, Term}.
validate(Term, {quz, _Value}, _Validators) -> {valid, Term}.

post_validate(Term, _Validators) -> valid.
```

It uses a function chaining technique to let you implement complex validation
rules easily. To understand how to implement custom validator in depth, refer
to the documentation.
