%json_parse(JSONString, Object) :- atom_chars(JSONString, Object).
%json_parse(JSONString, Object) :- atom_codes(JSONString, Object).
%json_parse(JSONString, Object) :- string_codes(JSONString, Object).
%json_parse(JSONString, Object) :- atom_string(JSONString, Object).

%json_parse(JSONString, Object) :- atom_chars(JSONString, Object).
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


a(A)-->[A].








integer(I) -->
        digit(D0),
        digits(D),
        { number_codes(I, [D0|D])
        }.

digits([D|T]) -->
        digit(D), !,
        digits(T).
digits([]) -->
        [].

digit(D) -->
        [D],
        { code_type(D, digit)
        }.




%%%%%%%%%%%%%%%%%%%%%%%




json(JSON) --> json_obj(JSON), ! ;
               json_array(JSON), ! ;
               json_number(JSON), ! ;
               json_string(JSON).


json_obj(json_object(Pairs)) --> open_brace,
                               sepBy(comma , pair, Pairs),
                               close_brace.

json_array(json_array(Array)) --> open_bracket,
                                  sepBy(comma, json, Array),
                                  close_bracket.



sepBy(Sep, P, [X | Xs]) --> call(P, X),
                            sepByAux(Sep, P, Xs).

sepBy(_, _, []) --> [].

sepByAux(Sep, P, [X | Xs]) --> call(Sep),
                               call(P, X),
                               sepByAux(Sep, P, Xs).

sepByAux(_, _, []) --> [].


pair((Key, Value)) --> json_string(Key),
                       colon,
                       json(Value).



json_number(N) --> integer(N),
                   spaces.

json_char(C) --> string(""), % escaped dquote
                 { [C] = "\"" }.

json_char(C) --> char(C),
                { [C] \= "\"" }. % any non dquote

spaces --> char(Space),
           { member(Space, " \t\n") },
           spaces.

spaces --> [].


char(C, [C | S], S).

string(S) --> foldl(char, S).



zero_or_more(Parser, List) --> one_or_more(Parser, List).

zero_or_more(_, []) --> [].



one_or_more(Parser, [One | More]) --> call(Parser, One),
                                      zero_or_more(Parser, More).


json_string(Name) --> dq,
                      zero_or_more(json_char, RawString),
                      dq,
                      spaces,
                     { string_codes(Name, RawString) }.




%pair(X,Y,(X,Y)).




json_obj2(json_obj([])) --> open_brace,
                           [],
                           close_brace, !.

json_obj2(json_obj([O])) --> open_brace,
                            members(O),
                            close_brace.

members(O) --> pair(O),
               comma,
               members(O), !.




json_array2(json_array([])) --> open_bracket,
                               [],
                               close_bracket, !.

json_array2(json_array([A])) --> open_bracket,
                                [A],
                                close_bracket.
dot --> [46].

dq --> [34].

sq --> [39].

colon --> [58].

comma --> [44].

open_brace --> [123].
close_brace --> [125].

open_bracket --> [91].
close_bracket --> [93].



json_parse(JSONString, JSON) :- atom_codes(JSONString, S),
                                 phrase(json(JSON), S).


file_name('JSONCaglio/prova.json').
json_load(FileName, JSON) :- file_name(FileName),
                             open(FileName, read, In),
                             read(In, JSONString),
                             close(In),
                             json_parse(JSONString, JSON).




%space(_S) --> [ ];[\n];[\r];[\t].
%space([]) --> [].
