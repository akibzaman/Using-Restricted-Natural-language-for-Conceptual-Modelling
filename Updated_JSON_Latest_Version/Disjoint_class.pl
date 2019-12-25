:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
:- consult(predicates). 

s([type:const_neg, sem:Sem]) -->
   np([num:N, type:const_neg, pos:subj, arg:X, sco:Sco, sem:Sem]),
   vp([num:N, type:const_neg, arg:X, sem:Sco]),
   ['.'].

np([num:N, type:const_neg, pos:P, arg:X, sco:Sco, sem:Sem]) -->
   det([num:N, type:const_neg, pos:P, arg:X, res:json(['Atom'=Res]), sco:Sco, sem:Sem]),
   noun([num:sg, type:const_neg, pos:P, arg:X, sem:Res]).

np([num:N, type:const_neg, pos:P, arg:X, sem:Sem]) -->
   det([num:N, type:const_neg, pos:P, arg:X, sco:Sco, sem:Sem]),
   noun([num:N, type:const_neg, pos:P, arg:X, sem:Sco]).

noun([num:N, type:const_neg, pos:subj, arg:X, sem:Sem]) -->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:no, arg:X, sem:Sem])}, WForm.
  
noun([num:N, type:const_neg, pos:obj, arg:X, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, init:no, arg:X, sem:Sem])}, WForm.
   

vp([num:N, type:const_neg, arg:X, sem:Sem]) -->   
    copula([wfrm:[is]]), 
    np([num:_N, type:const_neg, pos:obj, arg:X, sem:Sem]).

det([num:N, type:const_neg, pos:subj, arg:X, res:JsonHead, sco:JsonBody, 
					sem:json(['Forall'=json(['Var'=X, 
						'Implies'= json([head=JsonHead, body=JsonBody])])])]) --> ['No'].
   
det([num:N, type:const_neg, pos:obj, arg:X, sco:Sco,  
					sem:json(['Neg'=json(['Atom'=Sco])])]) --> [a].

copula([wfrm:[is]]) --> [is].

% ----------------------------------------------------------------------------------

lexicon([cat:noun, wform:[team, member], num:sg, type:entity, pos:subj, init:yes, arg:X, sem:json(['Rel'=entity, 'Ind'=team_member, 'Var'=X])]).   

lexicon([cat:noun, wform:[coach], num:sg, type:entity, pos:subj, init:no, arg:X, 
		sem:json(['Rel'=entity, 'Ind'=coach, 'Var'=X])]).

lexicon([cat:noun, wform:[player], num:sg, type:entity, pos:obj, init:no, arg:X, 
		sem:json(['Rel'=entity, 'Ind'=player, 'Var'=X])]).
		
lexicon([cat:noun, wform:[post, graduate, student], num:sg, type:entity, pos:obj, init:no, arg:X, 
		sem:json(['Rel'=entity, 'Ind'=post_graduate_student, 'Var'=X])]).


%lexicon([cat:copula, wform:[is], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=subclass_of, 'Var'=[Y, X]])]).

% ----------------------------------------------------------------------------------

test1 :-
   s([type:const_neg, sem:PrologTerm], 
   ['No', coach, is, a, player, '.'], []),
   prolog_vars_to_json_vars(PrologTerm, JsonTerm), 
   atom_json_term(JSON, JsonTerm, [as(atom)]),
   write(JSON).

test2 :-							   
	JSON = ' {
			  "Forall": {
				"Var":"V1",
				"Implies": {
				  "head": {"Atom": {"Rel":"entity", "Ind":"coach", "Var":"V1"}},
				  "body": {"Neg": {"Atom": {"Rel":"entity", "Ind":"player", "Var":"V1"}}}
				}
			  }
			}',
  atom_json_term(JSON, JSONTerm, [as(atom)]),
  json_vars_to_prolog_vars(JSONTerm, PrologVars),
  s([type:const_neg, sem:PrologVars], Sentence, []),!,
  nl, nl,
  writeq(Sentence).
