%json_parse(JSONString, Object) :- atom_chars(JSONString, Object).
%json_parse(JSONString, Object) :- atom_codes(JSONString, Object).
%json_parse(JSONString, Object) :- string_codes(JSONString, Object).
%json_parse(JSONString, Object) :- atom_string(JSONString, Object).

json_parse(JSONString, Object) :- atom_chars(JSONString, Object).
%                                  json_obj(JSONString, Object),
%                                  json_array(JSONString, Object).
%json_obj({},json_obj(_)).
%json_array([],json_array(_)).


digit_weight(W) -->
        [D],
        { code_type(D, digit(W)) }.


