json_load(FileName, JSON) :- file_name(FileName),
                             open(FileName, read, In),
                             read(In, JSONString),
                             close(In),
                             json_parse(JSONString, JSON).

file_name('JSONCaglio/prova.json').

json_parse(JSONString, JSON) :- atom_codes(JSONString, List),
                                phrase(json(JSON), List).

json(JSON) --> json_obj(JSON), !;
               json_array(JSON), !;
               json_number(JSON), !;
               json_string(JSON).
			   
json_obj(json_object(Pairs)) --> delete_space,
                                 open_brace,
                                 delete_space,
                                 divide(comma, pair, Pairs),
                                 delete_space,
                                 close_brace,
                                 delete_space.

json_array(json_array(Array)) --> delete_space,
                                  open_bracket,
                                  delete_space,
                                  divide(comma, json, Array),
                                  delete_space,
                                  close_bracket,
                                  delete_space.

json_number(N) --> delete_space,
                   number(N),
                   delete_space.
				   
json_string(S) --> delete_space,
                   string_parse_dq(S),
                   delete_space;
                   delete_space,
                   string_parse_sq(S),
                   delete_space.
				   
				   
divide(Sep, P, [X | Xs]) --> call(P, X),
                             divide_symbol(Sep, P, Xs).

divide(_, _, []) --> [].

divide_symbol(Sep, P, [X | Xs]) --> call(Sep),
                                    call(P, X),
                                    divide_symbol(Sep, P, Xs).

divide_symbol(_, _, []) --> [].

pair((Key, Value)) --> json_string(Key),
                       delete_space,
                       colon,
                       delete_space,
                       json(Value).
			   
			   
string_parse_dq(S) --> dq,
                       char_parse_dq(C0),
                       chars_parse_dq(C),
                       {
                          string_codes(S, [C0 | C])
                       }.

char_parse_dq(C) --> [C],
                     {
                         code_type(C, ascii)
                     }.

chars_parse_dq([]) --> dq, !.

chars_parse_dq([C | T]) --> char_parse_dq(C), !,
                            chars_parse_dq(T).

%chars_parse_dq([]) --> [].


string_parse_sq(S) --> sq,
                       char_parse_sq(C0),
                       chars_parse_sq(C),
                       {
                           string_codes(S, [C0 | C])
                       }.

char_parse_sq(C) --> [C],
                     {
                         code_type(C, ascii)
                     }.

chars_parse_sq([]) --> sq, !.

chars_parse_sq([C | T]) --> char_parse_sq(C), !,
                            chars_parse_sq(T).

%chars_parse_sq([]) --> [].


number(F) --> digit(D0),
              digits(D),
              {
                  number_codes(F, [D0 | D])
              }.

digits([46, D | T]) --> dot,
                        digit(D),
                        !,
                        digits(T).

digits([D | T]) --> digit(D), !,
                    digits(T).

digits([]) --> [].

digit(D) --> [D],
             {
                 code_type(D, digit)
             }.

delete_space --> space, delete_space;
                 tab, delete_space;
                 nl,delete_space;
                 cr,delete_space;
                 [].

tab --> [9].

nl --> [10].

cr --> [13].

space --> [32].

dq --> [34].

sq --> [39].

comma --> [44].

dot --> [46].

colon --> [58].

open_bracket --> [91].
close_bracket --> [93].

open_brace --> [123].
close_brace --> [125].

prova(JSONString, JSON, Elemento) :- json_parse(JSONString, JSON),
                                     Elemento is JSON.
