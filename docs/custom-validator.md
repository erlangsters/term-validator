# Implementing custom validators

To implement a custom validator, you must write a callback module implementing
the **term_validator** behavior. Then, in order to use this custom
validator, you call the usual `term_validator:validate/3` with the validator.

If you're implementing the "xyz" validator, your callback module typically is
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

(Usually, you write your own validate/2 function that includes all the custom
validators of your project.)

Having a look at the implementation of the built-in validators, while reading
this section, is not a bad idea.

## Validation functions chain

When a term is being validated, the validate functions of your callback module
are all called successively; the pre-validate function is first called, then
the validate function is called for each option, then the post-validate
function is last called. This chain of calls gives you plenty of flexibility
and allows you to build complex validation logic fairly easily.

During this chain, the validated term is being passed around and can
potentially be transformed. At any point during the chain, any of the functions
can interrupt it and return the reason why a term is invalid.

## The validator options

The custom validator must provide the list of options it supports. They come
in two flavors, the mandatory ones and the optional ones. They're specified
with the `options/1` callback function which is called with both the
`mandatory` atom or the `optional` atom.

```erlang
options(mandatory) ->
  [foo];
options(optional) ->
  [bar].
```

In this example, the `foo` option is mandatory and therefore, the term format
will require the `foo` option to be valid.

```erlang
Format = {xyz, [{foo, Value}]}.
```

Omitting it will cause the validate function to return
`{missing_options, [foo]}` early (with no additional work in the validator
implementation). The optional options can safely be omitted.

The previous example also implies that any other options are invalid and it
will cause the validate function to return `{invalid_options, [quz]}` for
instance, if you include the `quz` option to the term format (and again, with
no additional work in the validator implementation).

Additionally, if you validator must accept an arbitrary set of options, you can
return the `dynamic` atom.

```erlang
options(_) ->
    dynamic.
```

(It's used in the `any_of` and `all_of` validators.)

The options validation is then delegated to your `pre_validate/3` callback
function.

(Note that the value of the option itself is not checked at this level but may
be checked later during the validation process.)

## The pre-validate function

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

## The validate function

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

## The post-validate function

While hardly needed, this function is called last and allows you to do a last
check after all passed options have been processed.

If the term continues to be all valid, it returns the `valid` atom in order to
finalize the validation process. If the term is invalid, the
`{invalid, Reason}` tuple is returned with the reason.
