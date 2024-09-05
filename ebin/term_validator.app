{application, 'term_validator', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['all_of_validator','any_of_validator','any_validator','atom_validator','bool_validator','length_option_validator','list_validator','map_dynamic_validator','map_validator','number_validator','string_validator','term_validator','tuple_dynamic_validator','tuple_validator']},
	{registered, []},
	{applications, [kernel,stdlib]},
	{optional_applications, []},
	{env, []}
]}.