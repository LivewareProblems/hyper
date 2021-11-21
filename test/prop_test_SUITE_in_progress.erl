-module(prop_test_SUITE_in_progress).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [prop_set_case].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

prop_set_case(Config) ->
    ct_property_test:quickcheck(
        prop_hyper:prop_set(),
        Config
    ).
