tag(STRING, TAG) --> { atom_chars(STRING, TAG) },
  open_angle_brackets, word(TAG), close_angle_brackets.

endtag(TAG) -->
   "".

solotag(TAG) -->
   "<", word(TAG), "/>".

word(WORD) --> { var(WORD), ! },
   chars(CHARS), { atom_codes(WORD, CHARS) }.
word(WORD) --> { nonvar(WORD) },
   { atom_codes(WORD, CHARS) }, chars(CHARS).

chars([X|Y]) --> char(X), chars(Y).
chars([]) --> [].

char(X) --> [X], { is_char(X) }.

is_char(X) :- X >= 0'a, X =< 0'z, !.
is_char(X) :- X >= 0'A, X =< 0'Z, !.
is_char(X) :- X >= 0'0, X =< 0'9, !.
is_char(0'_).


open_angle_brackets --> [<].
close_angle_brackets --> [>].



json_obj --> open_brace,
             close_brace.
open_brace --> ['{'].
close_brace --> ['}'].



json_parse2(JSONString, Object) :-
    atom_chars(JSONString, Object),
    json_obj(Object, []).

