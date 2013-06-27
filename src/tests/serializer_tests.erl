-module(serializer_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

serialize_should_encode_content_as_JSON_by_default_test() ->
  Expected = [123, "\"source\"", 58, "\"OMDB\"", 44, "\"title\"", 58, "\"Fubar\"",
    44, "\"year\"", 58, "2007", 44, "\"rating\"", 58, "7.0", 125],
  Actual = serializer:serialize(
    {struct, [{"source", "OMDB"}, {"title", "Fubar"}, {"year", 2007}, {"rating", 7.0}]},
    whatever),
  ?assert(Actual =:= Expected).

serialize_should_encode_content_as_JSON_if_specified_type_is_JSON_test() ->
  Expected = [123, "\"source\"", 58, "\"OMDB\"", 44, "\"title\"", 58, "\"Fubar\"",
    44, "\"year\"", 58, "2007", 44, "\"rating\"", 58, "7.0", 125],
  Actual = serializer:serialize(
    {struct, [{"source", "OMDB"}, {"title", "Fubar"}, {"year", 2007}, {"rating", 7.0}]},
    json),
  ?assert(Actual =:= Expected).

deserialize_should_decode_JSON_content_by_default_test() ->
  Expected = {struct, [{"source", "OMDB"}, {"title", "Fubar"}, {"year", 2007}, {"rating", 7.0}]},
  Actual = serializer:deserialize(
    [123, "\"source\"", 58, "\"OMDB\"", 44, "\"title\"", 58, "\"Fubar\"", 44, "\"year\"", 58, "2007", 44, "\"rating\"", 58, "7.0", 125],
    whatever),
  ?assert(Actual =:= Expected).

deserialize_should_decode_JSON_content_if_specified_type_is_JSON_test() ->
  Expected = {struct, [{"source", "OMDB"}, {"title", "Fubar"}, {"year", 2007}, {"rating", 7.0}]},
  Actual = serializer:deserialize(
    [123, "\"source\"", 58, "\"OMDB\"", 44, "\"title\"", 58, "\"Fubar\"", 44, "\"year\"", 58, "2007", 44, "\"rating\"", 58, "7.0", 125],
    json),
  ?assert(Actual =:= Expected).

-endif.