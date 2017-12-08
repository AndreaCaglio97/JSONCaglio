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




sentence -->
  subject,
  verb,
  objects.

subject -->
  modifier,
  noun.

objects -->
  modifier,
  noun.

modifier --> [the].

noun --> [cat].
noun --> [mouse].
noun --> [polar, bear].

verb --> [chases].
verb --> [eats].




json_obj --> open_brace,
             close_brace.
open_brace --> ['{'].
close_brace --> ['}'].



json_parse2(JSONString, Object) :-
    atom_codes(JSONString, Object),
    json_obj(Object, []).
