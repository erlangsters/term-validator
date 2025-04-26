# Using the built-in validators

To validate an Erlang term, use the `term_validator:validate/3` function and
pass it the term, its format, and the validators used by the format.

This library is bundled with a validator for each built-in Erlang type. For
instance, the `number_validator` is responsible for validating an Erlang number
term.

Most of the time, you will want to use all the built-in validators at once
and therefore, you may use the `term_validator:validate/2` function instead as
it registers them for you.

```erlang
#{
    any => any_validator,
    atom => atom_validator,
    bool => bool_validator,
    number => number_validator,
    string => string_validator,
    list => list_validator,
    tuple => tuple_validator,
    tuple_dynamic => tuple_dynamic_validator,
    map => map_validator,
    map_dynamic => map_dynamic_validator,
    any_of => any_of_validator,
    all_of => all_of_validator
}.
```

Therefore, write your term format according the above naming.

## The 'any' validator

> The module implementing the any Erlang term validator is `any_validator` and
registered as `any` (if you use the `term_validator:validate/2` function).

It allows to validate "anything". You may wonder how it's useful.

Well, it turns out to be useful during development when the type is not known
yet, but most importantly, some larger data structures that nest other data
may be "user-defined".

```erlang
valid = term_validator:validate([false, 42, "Hello world!"], {list, [{item, any}]}).
```

The above example will validate any list.

## The 'atom' validator

> The module implementing the Erlang atom validator is `atom_validator` and
registered as `atom` (if you use the `term_validator:validate/2` function).

The built-in atom validator allows you to validate an atom which can, if
allowed, also be given in its string form. The atom can be restricted to a set
of valid values.

It supports the following options (all of them are optional).

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
{invalid, {not_one_of, [foo, bar]}} = term_validator:validate(abc, Format).
```

## The 'bool' validator

> The module implementing the Erlang bool validator is `bool_validator` and
registered as `bool` (if you use the `term_validator:validate/2` function).

The built-in bool validator allows you to validate a boolean value, which can,
if allowed, also be given in a number form (typically 0 or -1).

It only supports the `allow_number` option which is optional.

Basic example.

```erlang
valid = term_validator:validate(false, bool).
valid = term_validator:validate(true, bool).

{invalid, not_bool} = term_validator:validate(yolo, bool).

valid = term_validator:validate(0, {bool, [allow_number]}).
valid = term_validator:validate(-1, {bool, [allow_number]}).
```

## The 'number' validator

> The module implementing the Erlang number validator is `number_validator` and
registered as `number` (if you use the `term_validator:validate/2` function).

The built-in number validator allows you to validate both integral and
floating-point numbers. In fact, Erlang does not directly distinguish between
the two - it only has the 'number type'. However, it's still convenient to be
able to validate integers only, which is supported by the validator.

It supports the following options (all of them are optional).

- `min`
- `max`
- `integer_only`
- `multiple_of`

Basic example.

```erlang
valid = term_validator:validate(42, number),
valid = term_validator:validate(42.5, number),

{invalid, not_number} = term_validator:validate("42", number),

% Note that the specified minimum is inclusive.
{invalid, {must_be_greater_or_equal_to, 42}} =
  term_validator:validate(41, {number, [{min, 42}]}),
valid = term_validator:validate(42, {number, [{min, 42}]}),

% To switch to an exclusive minimum, just specify it.
{invalid, {must_be_strictly_greater_than, 42}} =
  term_validator:validate(42, {number, [{min, {42, exclusive}}]}),

{invalid, must_be_integer} = term_validator:validate(42.5, {number, [integer_only]}),
```

The above example is self-explanatory and shows everything you need to know.

## The 'string' validator

> The module implementing the Erlang string validator is `string_validator` and
registered as `string` (if you use the `term_validator:validate/2` function).

The built-in string validator allows you to validate strings. In Erlang, there
are no 'string type' as they're implemented as list (they're list of Unicode
codepoints). It also supports validating with a regex.

Do not use the string validator to validate a list. While it would work, it's
conceptually wrong to do it.

It supports the following options (all of them are optional).

- `alphabet`
- `length`
- `pattern`
- `ascii`
- `latin1`
- `pattern`

The `min` and `max` are shortcuts for `{length, {X, Y}}`, `latin1` is
shortcut for `{alphabet, latin1}` and `ascii` is shortcut for
`{alphabet, ascii}`. Therefore, they're mutually exclusive.

Note that it does not support validating binaries.

Basic example.

```erlang
valid = term_validator:validate("Hello world!", string).
{invalid, not_string} = term_validator:validate('Hello world', string).
```

The `length` option allows you to accept only a range.

```erlang
valid = term_validator:validate("abc", [length, {2, 128}]),
valid = term_validator:validate("abc", [length, {min, 2}]),
{invalid, {length, {must_be_strictly_greater_than, 2}}} = term_validator:validate("abc", [length, {min, 2, exclusive}]),
```

The `alphabet` option allows you to accept only a subset of characters. It
accepts either the list of allowed characters, or the `latin1` or `ascii`
atoms. For instance, if you need a string that is Latin-1 encodable, use
`{alphabet, latin1}`.

```erlang
valid = term_validator:validate("CTCACA", [{alphabet, "ATGC"}]),
{invalid, {wrong_character, 88, position, 4}} = term_validator:validate("CTCXCA", [{alphabet, "ATGC"}]),

{invalid, _} = term_validator:validate("こんにちは世界", [{alaphabet, latin-1}]).
```

The `pattern` option allows you to validate based on a regex. It uses the `re`
module from the standard library.

```erlang
Format = {string, [{pattern, "^Hello"}]}.

valid = term_validator:validate("Hello world!", Format).
{invalid, {pattern_mismatch, _}} = term_validator:validate("Bye world!", Format).
```

The above examples are self-explanatory and shows everything you need to know.

## The 'list' validator

> The module implementing the Erlang list validator is `list_validator` and
registered as `list` (if you use the `term_validator:validate/2` function).

The built-in list validator allows you to validate an Erlang list.

It supports the following options (one of them is mandatory).

- `item` (mandatory)
- `length`
- `min`
- `max`

The `item` option defines the valid values that can be in the list.

The `min` and `max` are shortcuts for `{length, {X, Y}}`. Therefore, they're
mutually exclusive.

Basic example.

```erlang
Format = {list, [{item, bool}]}.
valid = term_validator:validate([true, false, true], list).

{invalid, {items, [{2, not_bool}]}} =
  term_validator:validate([true, 42, false], Format).

% Examples of format restricting the size of list.
Format1 = {list, [{item, any}, {length, {42, 100}}]}.
Format2 = {list, [{item, any}, {min, 42}]}.
Format3 = {list, [{item, any}, {max, 100}]}.

% Remember, the 'item' option is mandatory and therefore the following will not
% work...
term_validator:validate([], list).

% However, to validate "any" list, use the 'any' term format.
term_validator:validate([foo, 42], [{item, any}]).
```

The above example is self-explanatory and shows everything you need to know.

## The 'tuple' validator

> The module implementing the Erlang tuple validator is `tuple_validator` and
registered as `tuple` (if you use the `term_validator:validate/2` function).

The built-in tuple validator allows you to validate an Erlang tuple of a fixed
number of elements. If you're looking to validate a tuple that has a variable
number of elements, look at the [dynamic tuple](#the-dynamic-tuple-validator)
validator.

It only has the mandatory `elements` option which is a list of term formats
defining the valid values of the tuple.

```erlang
Format = {tuple, [{elements, [bool, number, string]}]}.
valid = term_validator:validate({true, 42, "Hello world!"}, Format).

{invalid, {elements, [{2, not_number}]}} =
  term_validator:validate({true, false, "Hello world!"}, Format).
```

The above example is self-explanatory and shows everything you need to know.

## The 'dynamic tuple' validator

> The module implementing the dynamic Erlang tuple validator is
`tuple_dynamic_validator` and registered as `tuple_dynamic` (if you use the
`term_validator:validate/2` function).

The built-in dynamic tuple validator allows you to validate an Erlang tuple
with a variable number of elements.

It supports the following options (all of them are optional).

- `element`
- `length`
- `min`
- `max`

The `element` option defines the valid elements that can be in the tuple.

The `min` and `max` are shortcuts for `{length, {X, Y}}`. Therefore, they're
mutually exclusive.

```erlang
Format = {tuple_dynamic, [{element, any}]}.
valid = term_validator:validate({true, 42, "Hello world!"}, Format).
```

The above example is self-explanatory and shows everything you need to know.

## The 'map' validator

> The module implementing the Erlang map validator is `map_validator` and
registered as `map` (if you use the `term_validator:validate/2` function).

The built-in map validator allows you to validate Erlang maps with a known set
of, optional or mandatory, fields. If you're looking to validate a map that has
a variable number of fields, look at the
[dynamic map](#the-dynamic-map-validator) validator.

It only has the mandatory `fields` option which defines the list of key and
valid values, and whether they're optional or mandatory.

```erlang
Format = {map, [{fields, [
    {foo, number, optional},
    {"bar", string, mandatory}
]}]}.

valid = term_validator:validate(#{"bar" => 42}, Format).

{invalid, {missing_fields, ["bar"]}} =
  term_validator:validate(#{foo => 42}, Format).
```

Be careful, the key is specified as a **term** that needs to be matched, unlike
the value which is specified as a **term format**.

## The 'dynamic map' validator

The module implementing the dynamic Erlang map validator is
`map_dynamic_validator` and registered as `map_dynamic` (if you use the
`term_validator:validate/2` function).

The built-in dynamic map validator allows you to validate an Erlang map with a
variable number of fields.

It supports the following options (all of them are optional).

- `key`
- `value`
- `length`
- `min`
- `max`

The `key` and `value` options allow to restrict the map with a **term format**.
If they're not specified, any key and value are valid. The `min` and `max` are
shortcuts for `{length, {X, Y}}`. Therefore, they're mutually exclusive.

```erlang
valid = term_validator:validate(#{}, dynamic_map).

Format = {dynamic_map, [{key, string}]}.
valid = term_validator:validate(#{"Hello world!" => true}, Format).

{invalid, {keys, 42}} = term_validator:validate(#{42 => false}, Format).
```

The above example is self-explanatory and shows everything you need to know.

## The 'any of' validator

> The module implementing the "any of" validator is `any_of_validator` and
registered as `any_of` (if you use the `term_validator:validate/2` function).

The "any of" validator allows you to validate an Erlang term against a list of
term formats; if the term can be validated against one of the formats, it's
valid.

```erlang
Format = {any_of, [atom, string]}.

valid = term_validator:validate("Hello world", Format).
valid = term_validator:validate(hello_world, Format).
```

Observe how this validator is used with a list of term formats instead of a
list of options.

## The 'all of' validator

> The module implementing the "all of" validator is `all_of_validator` and
registered as `all_of` (if you use the `term_validator:validate/2` function).

The "all of" validator allows you to validate an Erlang term against a list of
term formats; if the term can be validated against all of the formats, it's
valid.


```erlang
Format1 = {number, [{min, 41}]},
Format2 = {number, [{max, 43}]},

valid = term_validator:validate(42, {all_of, [Format1, Format2]}).
```

Observe how this validator is used with a list of term formats instead of a
list of options.