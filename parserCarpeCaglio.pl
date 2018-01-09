json_load(FileName, JSON) :- open(FileName, read, In),
                             read(In, JSONString),
                             close(In),
                             json_parse(JSONString, JSON).

json_write(JSON, FileName) :-
                          open(FileName, write, Out),
                          (write_obj(JSON, String), !;
                          write_array(JSON, String), !),
                          concat("\'", String, String_rest),
                          concat(String_rest, "\'", String_file),
                          atom_string(Atom_file, String_file),
                          write(Out, Atom_file),
                          put(Out, 0'.),
                          close(Out).

write_obj(json_obj(Member), String_obj) :-
                            write_member(Member, String_member),
                            concat("{", String_member, String_rest),
                            concat(String_rest, "}", String_obj).

write_array(json_array(Element), String_array) :-
                           write_element(Element, String_element),
                           concat("[", String_element, String_rest),
                           concat(String_rest, "]", String_array).

write_member([], "").

write_member([Member], String_pair) :-
                                    write_pair(Member, String_pair).

write_member([Member | Members], String_member) :-
                 write_pair(Member, String_pair),
                 write_member(Members, String_members),
                 concat(String_pair, ",", String_rest),
                 concat(String_rest, String_members, String_member).

write_element([], "").

write_element([Element], String_value) :-
                                 write_value(Element, String_value).

write_element([Element | Elements], String_element) :-
               write_value(Element, String_value),
               write_element(Elements, String_elements),
               concat(String_value, ",", String_rest),
               concat(String_rest, String_elements, String_element).

write_pair(Pair, String_pair) :-
                     Pair = (String, Value),
                     write_string(String, String_final),
                     concat(String_final, ":", String_rest),
                     write_value(Value, String_value),
                     concat(String_rest, String_value, String_pair).

write_value(Value, String_value) :-
                                  write_obj(Value, String_value);
                                  write_array(Value, String_value);
                                  write_number(Value, String_value);
                                  write_string(Value, String_value).

write_string(String, String_final) :-
                                   concat("\"", String, Rest),
                                   concat(Rest, "\"", String_final).

write_number(Number, String_number) :-
                               number(Number),
                               number_string(Number, String_number).


json_parse(JSONString, JSON) :- atom_codes(JSONString, List),
                                phrase(json_start(JSON), List).

json_start(JSON) --> json_obj(JSON), !;
                     json_array(JSON), !.

json(JSON) --> json_obj(JSON), !;
               json_array(JSON), !;
               json_number(JSON), !;
               json_string(JSON).

json_obj(json_obj(Pairs)) --> delete_space,
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
                   delete_space, !;
                   delete_space,
                   string_parse_sq(S),
                   delete_space.


divide(Div, G, [X | Xs]) --> call(G, X),
                             divide_symbol(Div, G, Xs).

divide(_, _, []) --> [].

divide_symbol(Div, G, [X | Xs]) --> call(Div),
                                    call(G, X),
                                    divide_symbol(Div, G, Xs).

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


number(F) --> sign(S),
              digit(D0),
              digits(D),
              {
                  number_codes(F, [S, D0 | D])
              };
              digit(D0),
              digits(D),
              {
                  number_codes(F, [D0 | D])
              }.

sign(S) --> [S],
            {
                S = 45
            };
            [S],
            {
                S = 43
            };
            {
                fail
            }.

digits([46, D | T]) --> dot,
                        digit(D),
                        !,
                        digits_after_dot(T).

digits([D | T]) --> digit(D), !,
                    digits(T).

digits([]) --> [].

digits_after_dot([D | T]) --> digit(D), !,
                              digits_after_dot(T).

digits_after_dot([]) --> [].

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


json_get(json_obj(Members), [Field | Fields], Result) :-
    search_obj(Members, [Field], Result_rest),
    json_get(Result_rest, Fields, Result), !.

json_get(json_array(Elements), [Field | Fields], Result) :-
    search_array(Elements, Field, Result_rest),
    json_get(Result_rest, Fields, Result), !.

json_get(Result, [], Result).

json_get(json_obj(JSON_obj), Field, Result) :-
    search_obj(JSON_obj, Field, Result), !.

json_get(json_array(Array), I, Result) :-
    search_array(Array, I, Result), !.

json_get(JSON_obj, Field, Result) :-
    json_get(JSON_obj, [Field], Result).

search_obj([Member | Members], [Field], Result) :-
    Member = (Field, Result);
    search_obj(Members, [Field], Result).

search_obj([], _, _) :- fail.

search_array(Array, I, Element) :-
    number(I),
    length(Back, I),
    append(Back, [Element | _], Array).



