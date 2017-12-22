number(F) --> digit(D0),
              digits(D),
              {
                  number_codes(F, [D0|D])
              }.

digits([46,D|T]) --> dot,
                     digit(D),
                     !,
                     digits(T).

digits([D|T]) --> digit(D), !,
                  digits(T).

digits([]) --> [].

digit(D) --> [D],
             {
                 code_type(D, digit)
             }.



json(JSON) --> json_obj(JSON), ! ;
               json_array(JSON), ! ;
               json_number(JSON), ! ;
               json_string(JSON).


%json_obj(json_object(Pairs)) --> open_brace,
%                                 sepBy(comma , pair, Pairs),
%                                 close_brace.

json_obj(json_object([Pairs])) --> open_brace,
                                   pairs(Pairs),
                                   close_brace.


json_array(json_array(Array)) --> spazio,
                                  open_bracket,
                                  spazio,
                                  sepBy(comma, json, Array),
                                  spazio,
                                  close_bracket,
                                  spazio.



sepBy(Sep, P, [X | Xs]) --> call(P, X),
                            sepByAux(Sep, P, Xs).

sepBy(_, _, []) --> [].

sepByAux(Sep, P, [X | Xs]) --> call(Sep),
                               call(P, X),
                               sepByAux(Sep, P, Xs).

sepByAux(_, _, []) --> [].

pairs(Pairs) --> pair(Pairs),
                 comma,
                 pairs(Pairs);
                 [].

pair((Key, Value)) --> json_string(Key),
                       colon,
                       json(Value).



json_number(N) --> number(N).


%json_char(C) --> string(""), % escaped dquote
%                 { [C] = "\"" }.

%json_char(C) --> char(C),
%                { [C] \= "\"" }. % any non dquote

json_char(C) -->
    string("\\\""), % escaped dquote

    { [C] = "\"" }.

json_char(C) -->
    char(C),

    { [C] \= "\"" }. % any non dquote


char(C, [C | S], S).

string(S) --> foldl(char, S).



zero_or_more(Parser, List) --> one_or_more(Parser, List).

zero_or_more(_, []) --> [].



one_or_more(Parser, [One | More]) --> call(Parser, One),
                                      zero_or_more(Parser, More).


json_string(_Name) --> dq,
                      [_Name],
                      dq.
                  %  { string_codes(Name, _RawString) }.




%pair(X,Y,(X,Y)).



dot --> [46].

space --> [32].

tab --> [9].

nl --> [10].

cr --> [13].

dq --> [34].

sq --> [39].

colon --> [58].

comma --> [44].

open_brace --> [123].
close_brace --> [125].

open_bracket --> [91].
close_bracket --> [93].



json_parse(JSONString, JSON) :- atom_codes(JSONString, List),
                                phrase(json(JSON), List).


file_name('JSONCaglio/prova.json').
json_load(FileName, JSON) :- file_name(FileName),
                             open(FileName, read, In),
                             read(In, JSONString),
                             close(In),
                             json_parse(JSONString, JSON).


delete_space([],[]).

delete_space([X | Lista],Resto):- X = 32, !,
                                  delete_space(Lista,Resto).

delete_space([X | Lista],Resto):- X = 9, !,
                                  delete_space(Lista,Resto).

delete_space([X | Lista],Resto):- X = 10, !,
                                  delete_space(Lista,Resto).

delete_space([X | Lista],Resto):- X = 13, !,
                                  delete_space(Lista,Resto).

delete_space(Resto,Resto).



%delete_space([],[]).
%delete_space([X|Xs],Ys) :- X = 32,
%                           delete_space(Xs,Ys), !;
%                           X = 9,
%                           delete_space(Xs,Ys), !;
%                           X = 10,
%                           delete_space(Xs,Ys), !;
%                           X = 13,
%                           delete_space(Xs,Ys), !.

%delete_space([X|Xs],[X|Ys]) :- delete_space(Xs,Ys).



prova(X,Y) :- atom_codes(X, Y),
              delete_space(Y,Y).
              %atom_string(Y2,Y).

spazio --> space,spazio;[].
