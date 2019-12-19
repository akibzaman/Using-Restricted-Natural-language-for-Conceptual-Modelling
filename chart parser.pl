% ----------------------------------------------------------------
% Top-down Chart Parser with Lookahead Information
% Based on Gazdar and Mellish;
% Lookahead mechanism added by Rolf Schwitter
% Date: 2018-11-10
% ----------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).

% :- op(1200, xfx, --->).

:- dynamic edge/5, lookahead/4.


% -----------------------------------------------------------------------
% term_expansion/2
% transfors a definite clause gammar into a format that can be processed
% by the chart parser
% -----------------------------------------------------------------------

term_expansion((LHS --> RHS), Format) :-
  (
     RHS    = ( { lexicon(List) }, TokenList )
     ->
     Format = ( rule(LHS, [ lexicon(List) ]) )
  ;
     RHS    = ( TokenList, { lexicon(List) } )
     ->
     Format = ( rule(LHS, [ lexicon(List) ]) )
  ;
     conjunction_to_list_rhs(RHS, RHSList)
     ->
     Format = ( rule(LHS, RHSList) )
  ;
     Format = ( rule(LHS, RHS ) )
  ).

conjunction_to_list_rhs(RHS, RHSList) :-
  \+ is_list(RHS), 
  conjunction_to_list(RHS, RHSList).

conjunction_to_list((Term, Terms), [Term|ListofTerms]) :- !, 
  nonvar(Term), 
  conjunction_to_list(Terms, ListofTerms).

conjunction_to_list(Term, [Term]).


% ----------------------------------------------------------------
% test/1
% allows use of test sentences with chart parsers
% ----------------------------------------------------------------

test(Number) :-
   test_sentence(Number, TokenList),
   initialise_chart_parser(V0, LHS),
   chart_parser(V0, Vn, TokenList),
   foreach(edge(V0, Vn, LHS, [], Parse), mwrite(Parse)).


initialise_chart_parser(V0, LHS) :-
   retractall(edge(_, _, _, _, _)),
   retractall(lookahead(_, _, _, _)),
   V0 = 0,
   LHS = s([type:const, sem:Sem]),           % Start Symbol ([type:const, sem:Sem])
   nl, 
   write('Initialse chart: '),
   nl, nl,
   start_active(V0, LHS).


% ----------------------------------------------------------------
% chart_parser/3
% processes sentence on a token level and starts the chart for
% each token, producing lookahead categories for that token.
% ----------------------------------------------------------------

chart_parser(V, V, []).

chart_parser(V0, Vn, [Token|Tokens]) :-
   nl, nl,
   write('Process Token: '),
   write(Token),
   nl, nl,
   start_chart(V0, V1, Token),
   chart_parser(V1, Vn, Tokens).


% ----------------------------------------------------------------
% start_chart/3
% uses add_edge (defined by particular chart parser) to insert
% inactive edges for the tokens (and their respective categories)
% into the chart
% ---------------------------------------------------------------- 

start_chart(V0, V1, Token) :-
   V1 is V0 + 1,
   foreach(word(Category, Token), 
   add_edge(V0, V1, Category, [], [Token, Category])).


% ----------------------------------------------------------------
% Add active edges of type Category at vertex V0 by looking up
% the rules which expand Category in the grammar
% ----------------------------------------------------------------

start_active(V0, Category) :-
   foreach(rule(Category, Categories), 
   add_edge(V0, V0, Category, Categories, [Category])).

add_edge(V1, V2, Category, Categories, Parse) :-
   edge(V1, V2, Category, Categories, Parse), !.

add_edge(V1, V2, Category1, [], Parse) :-
   assert_edge(V1, V2, Category1, [], Parse), 
   foreach(edge(V0, V1, Category2, [Category1|Categories], Parses), 
           add_edge(V0, V2, Category2, Categories, [Parse|Parses])).

add_edge(V1, V2, Category1, [Category2|Categories], Parses) :-
   assert_edge(V1, V2, Category1, [Category2|Categories], Parses), 
   foreach(edge(V2, V3, Category2, [], Parse), 
           add_edge(V1, V3, Category1, Categories, [Parse|Parses])), 
   start_active(V2, Category2).


% ----------------------------------------------------------------
% assert_edge/5
% asserta(edge(...)),  but gives option of displaying nature of
% edge created
% ----------------------------------------------------------------

assert_edge(V1, V2, Category1, [], Parse1) :-
   asserta(edge(V1, V2, Category1, [], Parse1)),
   dbgwrite(inactive(V1, V2, Category1)).

assert_edge(V1, V2, Category1, [Category2|Categories], Parse1) :-
   asserta(edge(V1, V2, Category1, [Category2|Categories], Parse1)),
   dbgwrite(active(V1, V2, Category1, [Category2|Categories])),
   lookahead_categories(V1, V2, Category2).
   

% ----------------------------------------------------------------
% word/2
% ----------------------------------------------------------------

% word(Category, Token) :-
%   (Category ---> [Token]).


word(Category, W) :-
   rule(Category, [lexicon([cat:Cat, wform:[W|Ws]|Rest])]),
   call( lexicon([cat:Cat, wform:[W|Ws]|Rest]) ).
   
% word(Category, Token) :- 
%   (Category ---> {lexicon([cat:qnt,wform:Token,_,_,_,_,_])}, Token).


/*
% ----------------------------------------------------------------
% rule/2
% ----------------------------------------------------------------

rule(Mother, List_of_daughters) :-
   rule(Mother, Daughters),
   \+ is_list(Daughters), 
   conjtolist(Daughters, List_of_daughters).

% ----------------------------------------------------------------
% conjtolist - convert a conjunction of terms to a list of terms
% ----------------------------------------------------------------

conjtolist((Term, Terms),  [Term|List_of_terms]) :- !, 
   conjtolist(Terms, List_of_terms).

conjtolist(Term, [Term]).
*/

% ----------------------------------------------------------------
% foreach - for each X do Y
% ----------------------------------------------------------------

foreach(X, Y) :-
X, 
do(Y), 
fail.
  
foreach(X, Y) :-
true.
  
do(Y) :- Y, !.
  

% ----------------------------------------------------------------
% mwrite/1 prints out the mirror image of a tree encoded as a list
% ----------------------------------------------------------------

mwrite(Tree) :-
   nl,
   mirror(Tree, Image), 
   write(Image), 
   nl.


% ----------------------------------------------------------------
% mirror/2 - produces the mirror image of a tree encoded as a list
% ----------------------------------------------------------------

mirror([], []) :- !.

mirror(Atom, Atom) :-
   atomic(Atom).

mirror([X1|X2], Image) :-
   mirror(X1, Y2), 
   mirror(X2, Y1), 
   append(Y1, [Y2], Image).


% ----------------------------------------------------------------
% dbgwrite - a switchable tracing predicate
% ----------------------------------------------------------------

dbgwrite(Term) :-
   dbgon,
   write(Term),
   nl, !.

dbgwrite(Term).

dbgwrite(Term,Var) :-
   dbgon,
   integer(Var),
   tab(3 * (Var - 1)),
   write(Term),
   nl, !.

dbgwrite(Term,Var) :-
   dbgon,
   write(Term), write(" "), write(Var),
   nl, !.

dbgwrite(Term,Var).

dbgon.  % retract this to switch dbg tracing off


% ----------------------------------------------------------------
% lookahead_categories/3
% ----------------------------------------------------------------

lookahead_categories(V1, V2, Category) :-
   (
      lookahead(V1, V2, Category, LAHs)
      ->
      true  
   ;
      findall([V1, V2, Category, Token], word(Category, Token), LAHs),
      (
        LAHs = []
        ->
        true
      ;
        asserta(lookahead(V1, V2, Category, LAHs)),
        nl,
        write('Lookahead Categories: '),
        write(LAHs),
        nl, nl
      )
   ).




/*

% ----------------------------------------------------------------
% A set of test examples 
% ----------------------------------------------------------------

test_sentence(1, [kim, died]).

test_sentence(2, [sandy, saw, a, duck]).

test_sentence(3, [kim, knew, sandy, knew, lee, died]).

test_sentence(4, [the, woman, gave, a, duck, to, her, man]).

test_sentence(5, [lee, handed, a, duck, that, died, to, the, woman]).

*/

%test_sentence(6, ['Every', student, 'is enrolled in', 'exactly 1', program, '.']).

test_sentence(6, ['Every', student, is, enrolled, in, exactly, 1, program, '.']).

test_sentence(7, X).




s([type:const, sem:Sem]) -->               
  np([num:N, type:const, pos:subj, arg:X, sco:S, sem:Sem]),
  vp([num:N, type:const, arg:X, sem:S]), 
  fs.

fs --> { lexicon([cat:fs, wform:WForm]) }, WForm.
    
np([num:N, type:const, pos:subj, arg:X, sco:S, sem:Sem]) -->                 
  qnt([num:N, type:const, arg:X, res:json(['Atom'=R]), sco:S, sem:Sem]),
  noun([num:N, type:const, pos:subj, arg:X, sem:R]).                          
    
np([num:_N, type:const, pos:obj, arg:X, sco:S, sem:Sem]) -->       
  cst([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]),
  noun([num:N, type:const, pos:obj, arg:X, sem:R]).                          

vp([num:N, type:const, arg:X, sem:Sem]) -->
  verb([num:N, type:const, arg:X, arg:Y, sem:S]),
  np([num:_N, type:const, pos:obj, arg:Y, sco:S, sem:Sem]).

qnt([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 
  { lexicon([cat:qnt, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.
	
cst([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 					
  { lexicon([cat:cst, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.

noun([num:N, type:const, pos:subj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:no, arg:X, sem:Sem]) }, WForm.
  
noun([num:N, type:const, pos:obj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem]) }, WForm.

verb([num:N, type:const, arg:X, arg:Y, sem:Sem]) -->    
  { lexicon([cat:verb, wform:WForm, num:N, arg:X, arg:Y, sem:Sem]) }, WForm.


lexicon([cat:qnt, wform:['Every'], num:sg, arg:X, res:JsonHead, sco:JsonBody,
         sem:json(['Forall'=json(['Var'=X, 
                                   'Implies'= json([head=JsonHead,
                                                    body=JsonBody])])]) ]).

lexicon([cat:cst, wform:['exactly 1'], num:sg, arg:X, res:Res, sco:Sco,
         sem:json(['Exists'=json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         Res ]
                                       ])])]) ]).

lexicon([cat:cst, wform:[exactly, 1], num:sg, arg:X, res:Res, sco:Sco,
         sem:json(['Exists'=json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         Res ]
                                       ])])]) ]).
									   
lexicon([cat:cst, wform:[at, least, L, and, at, most, M], num:pl, arg:X, res:Res, sco:Sco,
         sem:json(['Exists'=json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'=L])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'=M])]),
                                         Res ]
                                       ])])]) ]). % :- atom_number(L1, L), atom_number(M1, M).
									   
lexicon([cat:cst, wform:[1, or, more], num:sg, arg:X, res:Res, sco:Sco,
         sem:json(['Exists'=json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:string','__text'='unbound'])]),
                                         Res ]
                                       ])])]) ]).									   
									   

lexicon([cat:noun, wform:[student], num:sg, type:entity, pos:subj, init:no, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]). 

lexicon([cat:noun, wform:[program], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=program, 'Var'=X])]).   

lexicon([cat:noun, wform:[undergraduate, student], num:sg, type:entity, pos:subj, init:no, arg:X, sem:json(['Rel'=entity, 'Ind'=undergraduate_student, 'Var'=X])]).   
             
lexicon([cat:noun, wform:[department], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 

lexicon([cat:noun, wform:[student], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]). 

lexicon([cat:noun, wform:[departments], num:pl, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 
                         
%lexicon([cat:verb, wform:['is enrolled in'], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=enrolled_in, 'Var'=[X, Y]]) ]).  

lexicon([cat:verb, wform:[is, enrolled, in], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=enrolled_in, 'Var'=[X, Y]]) ]).  

lexicon([cat:verb, wform:[is, a, subclass, of], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=subclass_of, 'Var'=[X, Y]]) ]).  

lexicon([cat:fs, wform:['.']]).


/*

% ----------------------------------------------------------------
% chart_parser/4
% processes sentence on a token level and starts the chart for
% each token
% ----------------------------------------------------------------

chart_parser(SNum, Vn, Vn, []).

chart_parser(SNum, V0, Vn, [Token|Tokens]) :-
   start_chart(SNum, V0, V1, [Token]),
   chart_parser(SNum, V1, Vn, Tokens).

start_chart(SNum, V0, V1, Token) :-
   V1 is V0 + 1,
   foreach(token(SNum, V0, V1, Category, Token), add_edge(SNum, V0, V1, Category, [])).

% ----------------------------------------------------------------
% token/6
% ----------------------------------------------------------------

token(compound, SNum, V1, V2, Category, [Token]) :-
   edge(SNum, V0, V1, Category, [ lexicon([cat:Cat, wform:[Token], struc:compound]) ]),
   add_edge(SNum, V0, V2, Category, []).

token(compound, SNum, V1, V2, Category, [Token]) :-
   edge(SNum, V0, V1, Category, [ lexicon([cat:Cat, wform:[Token|Tokens], struc:compound]) ]),
   assert_edge(SNum, V0, V2, Category, [ lexicon([cat:Cat, wform:Tokens, struc:compound]) ] ).

token(compound, SNum, V1, V2, Category, [Token]) :-
   call( lexicon([cat:Cat, wform:[Token|Tokens]|Rest]) ),
   call( rule(Category, [ lexicon([cat:Cat, wform:[Token|Tokens]|Rest]) ]) ),
   assert_edge(SNum, V1, V1, Category, [ lexicon([cat:Cat, wform:[Token|Tokens], struc:compound]) ]),
   assert_edge(SNum, V1, V2, Category, [ lexicon([cat:Cat, wform:Tokens, struc:compound]) ]).

token(simple, SNum, V1, V2, Category, [Token]) :-
   call( lexicon([cat:Cat, wform:[Token]|Rest]) ),
   call( rule(Category, [ lexicon([cat:Cat, wform:[Token]|Rest]) ]) ).

token(simple, SNum, V1, V2, Category, [Token]) :-
   call( rule(Category, [Token]) ).


*/
