# Term Validator - Documentation

This is the documentation of the
[Term Validator](https://github.com/erlangsters/term-validator) project. It
explains how to use the built-in validators and how to implement your own ones.

**Table of Contents**

- Using the built-in validators
    - The 'any' validator
    - The 'atom' validator
    - The 'bool' validator
    - The 'number' validator
    - The 'string' validator
    - The 'list' validator
    - The 'tuple' validator
    - The 'map' validator
    - The 'any_of' validator
    - The 'all_of' validator
- Implementing custom validators
    - Validation functions chain
    - The validator options
    - The pre-validate function
    - The validate function
    - The post-validate function

To be written.

## Using the built-in validators

The built-in validatrs are automatically included when using the `validate/2`
function. However, it you implement your own validators, you will need to
extend the list of validators to use during validation, and use the
`validate/3` function instead.

### The 'atom' validator

The built-in atom validator allows you to validate an atom which can, if
allowed, also be given in its string form. The atom can be restricted to a set
of valid values.

It has no mandatory options and features the following options.

- `one_of`
- `allow_string`

Basic example.

```erlang
valid = term_validator:validate(yolo, atom).
{invalid, not_atom} = term_validator:validate("yolo", atom).
```

To allow string, use the `allow_string` option.

```erlang
valid = term_validator:validate("yolo", {atom, [allow_string]}).
```

To restrict the atom to a set of valid values, use the `one_of` option.

```erlang
Format = {atom, [{one_of, [foo, bar]}]}.
valid = term_validator:validate(foo, Format).
valid = term_validator:validate(bar, Format).
{invalid, {not_one_of, [foo, bar]}} = term_validator:validate(quz, Format).
```

### The 'bool' validator

The built-in bool validator allows you to validate a boolean value, which can,
if allowed, also be given in a number form (typically 0 or -1).

It has no mandatory options and features only the `allow_number` option.

Basic example.

```erlang
valid = term_validator:validate(false, bool).
valid = term_validator:validate(true, bool).

{invalid, not_bool} = term_validator:validate(yolo, bool).

valid = term_validator:validate(0, {bool, [allow_number]}).
valid = term_validator:validate(-1, {bool, [allow_number]}).
```

### The 'number' validator

To be written.

### The 'string' validator

The built-in string validator allows you to validate strings. In Erlang, there
are no string type as they're implemented as list. Strings are subset of list
which should onlly contain integers of a valid range. Do not use the 'string'
validator to validate a list.  It also supports regex.

It has no mandatory options and features the following options.

- `alphabet`
- `length`
- `pattern`

Basic example.

```erlang
valid = term_validator:validate("Hello world!", string).
{invalid, not_string} = term_validator:validate(hello_world, string).
```


```erlang
valid = term_validator:validate("foo", [{alaphabet, utf-8}]),
valid = term_validator:validate("foo", [{alaphabet, ascii}]),
valid = term_validator:validate("foo", [{alaphabet, "abc"}]),
```

```erlang
valid = term_validator:validate("foo", [length, {2, 128}]),
valid = term_validator:validate("foo", [length, {min, 2}]),
valid = term_validator:validate("foo", [length, {min, 2, exclusive}]),
valid = term_validator:validate("foo", [length, {max, 2}]),
valid = term_validator:validate("foo", [length, {max, 2, exclusive}]),
valid = term_validator:validate("foo", [length, {2, 128}]),
valid = term_validator:validate("foo", [length, {2, 128}]),
```

### The 'list' validator

To be written.

### The 'tuple' validator

To be written.

### The 'map' validator

To be written.

### The 'any_of' validator

To be written.

### The 'all_of' validator

To be written.

## Implementing custom validators

To implement a custom validator, you must write a callback module implementing
the **term_validator** behavior. Then, in order to use this custom
validator, you call the usual `term_validator:validate/3` with the validator.

If you're implementing the 'xyz' validator, your callback module typically is
named `xyz_validator` and calling the validate function will ressemble this.

```erlang
term_validator:validate(Term, xyz, #{xyz => xyz_validator}).
```

But in practice, Erlang terms are nested and therefore you want to combine
your validator with the list of built-in validators. Consider the following.

```erlang
Format = {list, [{item, xyz}]}.
Validators = maps:merge(term_validator:validators(), #{xyz => xyz_validator}).

term_validator:validate(Term, Format, Validators).
```

Because it's a tedious syntax, you will usually write your own validate
function that will hide this details away and will include all the custom
validators of your project.

Having a look at the implementation of the built-in validators, while reading
this section, is not a bad idea.

### Validation functions chain

When a term is being validated, the validate functions of your callback module
are all called successively; the pre-validate function is first called, then
the validate function is called for each option, then the post-validate
function is last called. This chain of calls gives you plenty of flexibility
and allows you to build complex validation logic fairly easily.

During this chain, the validated term is being passed around and can
potentially be transformed. At any point during the chain, any of the functions
can interrupt it and return the reason why a term is invalid.

### The validator options

The custom validator must provide the list of options it supports. They come
in two flavors, the mandatory ones and the optional ones.

The mandatory options are specified with the `mandatory_options/0` callback
which returns the list of mandatory options. For instance, if you make the
`foo` option mandatory, the term format will require to include the `foo`
option and will therefore look like this: `{xyz, [{foo, Value}]}`.

```erlang
mandatory_options() -> [foo].
```

This will cause the validate function to return `{missing_options, [foo]}`
early (with no additional work in the validator implementation) if the option
is not included in the format.

The optional options are specified with the `options` callback which returns
the list of optional options.

```erlang
options() -> [bar]
```

This will cause the validate function to return `{invalid_options, [quz]}` for
instance, if you include the `quz` option to the term format, which is not
recognized by your validator (and again, with no additional work in the
validator implementation).

Additionally, if you validator must accept an arbitrary set of options, you can
return the `dynamic` atom.

```erlang
options() -> dynamic.
```

The options validation is then delegated to your `pre_validate/3` callback
function.

(Note that the value of the option itself is not checked at this level but may
be checked later during the validation process.)

### The pre-validate function

The `pre_validate/x` function is first called and is usually used to check the
type of the Erlang term.

```erlang
pre_validate(Term, Options, Validators) ->
    {valid, Term, Options}.
```

It's called with the term to validate, the options as specified in the format
and all the validators it should work with (relevant when validating nested
Erlang terms such a lists, tuples and maps). Note that the term and the options
can be transformed and passed to the next functions.

At this point, the validator is able to start detecting an invalid term. It
returns the `{invalid, Reason}` tuple along with the reason to interrupt the
validation process and have the validate function returns the reason.

```erlang
pre_validate(Term, Options, Validators) ->
    {invalid, "not valid because of this and that"}.
```

Note that if the `options/0` callback returns the `dynamic` atom, this function
should also check for options validity and return `{invalid_options. [quz]}`
for instance if it was passed with the `quz` option.

### The validate function

For all options specified in the format, the `validate/3` function is called
and returns either the `{valid, Term}` or `{invalid, Reason}` values. It allows
you to implement logic on a per-option basis easily.

Additionally, the function can also return either `{valid, Term, skip}` to
skip the processing of all the remaining options, or
`{valid, Term, skip, [foo, bar]}` to skip the processing of some remaining
options. Note that incorrect option values will be ignored.

Finally, if the value of the option itself is incorrect, it also needs to be
reported. The function must return the `{invalid_option_value, Reason}` tuple
which is then returned by the validate function.

### The post-validate function

While hardly needed, this function is called last and allows you to do a last
check after all passed options have been processed.

If the term continues to be all valid, it returns the `valid` atom in order to
finalize the validation process. If the term is invalid, the
`{invalid, Reason}` tuple is returned with the reason.
