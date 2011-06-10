-ifdef(PROPER).
-ifndef(PROPER_NO_SPECS).
proper_specs_test() ->
    ?assertEqual([], proper:check_specs(?MODULE, [long_result])).
-endif.
proper_module_test() ->
    ?assertEqual([], proper:module(?MODULE, [long_result])).
-endif.
