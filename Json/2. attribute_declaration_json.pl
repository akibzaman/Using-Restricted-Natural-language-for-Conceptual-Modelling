%------------------------------------------------------------------------
% Data Type Declaration:- Student id is of integer/string/date data type.
%------------------------------------------------------------------------

:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
:-consult(p_to_j).

:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(800, yfx, '#'). 


s([mode:M, type:attribute, sem:json(['Atom'=Sem])]) --> 
  np([mode:M, num:N, type:attribute, dt:DT, sem:Sem]), 
  [is, of, DT, data, type, '.'].

np([mode:M, num:N, type:attribute, dt:DT, sem:Sem]) -->
   n([mode:M, num:N, type:attribute, dt:DT, sem:Sem]).
   

n([mode:proc, num:N, type:attribute, dt:DT, sem:Sem])-->
   lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem]).
   
n([mode:gen, num:N, type:attribute, dt:DT, sem:Sem])-->
   { lexicon([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem]) },
   WForm. 

%---------------------------------------------------------------------------------------------

lexical_rule([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:_X, sem:Sem], L1, L2):-
   process_attribute([wform:WForm, dt:DT, arg:X, sem:Sem], L1, L2),
   assert(lexicon([cat:noun, wform:WForm, num:sg, type:attribute, dt:DT, arg:X, sem:Sem])).
   
process_attribute([wform:List3, dt:D, arg:X, sem:Sem], List1, List2):-
   (D = integer; D = string; D = date; D = boolean),
   append(List3, [is, of, D, data, type, '.'], List1),
   List2 = [is, of, D, data, type, '.'],
   lower_case_first_atom(List3, List4),
   atomic_list_concat(List4, '_', List5), V = ['Type'=D, 'Text'=X],
   Sem =json(['Rel'=attribute, 'Ind'=List5, 'Var'=json(V)]). % type added!

%lower_case_first_atom([Atom1|Rest], [Atom2|Rest]) :-
%   atom_codes(Atom1, [Char|Chars1]),
%   to_lower(Char, LowerChar),
%   atom_codes(Atom2, [LowerChar|Chars1]).

test1 :-
    s([mode:proc, type:attribute, sem:PrologTerm],
      ['Student', name, is, of, string, data, type, '.'], []),
    %numbervars(Sem),
	%JSONTerm = json(['Atom'=Sem]),
	prolog_vars_to_json_vars(PrologTerm, JSONTerm),
    atom_json_term(JSON, JSONTerm, [as(atom)]),
    write(JSON).
	
test2 :-
	JSON = '{
			  "Atom": {
				"Rel":"attribute",
				"Ind":"student_name",
				"Var": {"Type":"string", "Text":"V1"}
			  }
			}',
	atom_json_term(JSON, JSONTerm, [as(atom)]),
	%JSONTerm = json(['Atom'=Sem]),
    s([mode:gen, type:attribute, sem:JSONTerm], S, []), write(S), nl, nl.