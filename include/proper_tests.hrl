-ifdef(PROPER).
proper_specs_test() ->
    ?assertEqual([], proper:check_specs(?MODULE)).
proper_module_test() ->
    ?assertEqual([], proper:module(?MODULE)).
-endif.
