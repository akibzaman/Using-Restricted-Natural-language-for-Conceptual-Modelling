:- use_module(library(http/json)).
:- style_check([-discontiguous, -singleton]).
:- consult(predicates).
   
s([type:const, sem:Sem]) -->               
  np([num:N, type:const, pos:subj, arg:X, arg:Y, sco:S, sem:Sem]),
  vp([num:N, type:const, arg:X, arg:Y, sem:S]), ['.'].
    
np([num:N, type:const, pos:subj, arg:X, arg:Y, sco:S, sem:Sem]) -->                 
  det([num:N, type:const, arg:X, res:R, sco:json(['Var'=Y, 'Neg'=S]), sem:Sem]),
  noun([num:N, type:const, pos:subj, arg:X, sem:R]).                          
    
np([num:_N, type:const, pos:obj, arg:X, sco:S, sem:Sem]) -->       
  det([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]),
  noun([num:N, type:const, pos:obj, arg:X, sem:R]).                          

vp([num:N, type:const, arg:X, arg:Y, sem:json(['And'=[S1,S3]])]) -->
  verb([num:N, type:const, arg:X, arg:Y, sem:S]),
  np([num:_N, type:const, pos:obj, arg:Y, sco:S, sem:S1]),
  cc([wfrm:[and]]),
  verb([num:N, type:const, arg:X, arg:Y, sem:S2]),
  np([num:_N, type:const, pos:obj, arg:Y, sco:S2, sem:S3]).

det([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 
  { lexicon([cat:det, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.
	
det([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 					
  { lexicon([cat:det, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.

noun([num:N, type:const, pos:subj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:subj, init:no, arg:X, sem:Sem]) }, WForm.
  
noun([num:N, type:const, pos:obj, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:obj, arg:X, sem:Sem]) }, WForm.

verb([num:N, type:const, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:WForm, num:N, arg:X, arg:Y, sem:Sem]) }, WForm.

cc([wfrm:[and]]) --> [and].

lexicon([cat:det, wform:['No'], num:sg, arg:X, res:JsonHead, sco:JsonBody,
         sem:json(['Forall'=json(['Var'=X, 
                                   'Implies'= json([head=JsonHead,
                                                    body=JsonBody])])]) ]).				

lexicon([cat:det, wform:[a], num:sg, arg:X, res:Res, sco:Sco,
         sem:json(['Atom'=[ Sco, Res ]
                                       ])]).													
									   

lexicon([cat:noun, wform:[student], num:sg, type:entity, pos:subj, init:no, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   
             
lexicon([cat:noun, wform:[department], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 

lexicon([cat:noun, wform:[program], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=program, 'Var'=X])]). 

lexicon([cat:noun, wform:[unit], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=unit, 'Var'=X])]). 

lexicon([cat:noun, wform:[units], num:pl, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=unit, 'Var'=X])]). 

lexicon([cat:noun, wform:[departments], num:pl, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 
                         
lexicon([cat:verb, wform:[belongs, to], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=belong_to, 'Var'=[X, Y]]) ]).  

lexicon([cat:verb, wform:[is, enrolled, in], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=enrolled_in, 'Var'=[X, Y]]) ]).  

lexicon([cat:verb, wform:[studies], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=study, 'Var'=[X, Y]]) ]). 

lexicon([cat:verb, wform:[teaches], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=teach, 'Var'=[X, Y]]) ]).  


test1 :-
    s([type:const, sem:PrologTerm],
      ['No', student, studies, a, unit, and, teaches, a, unit, '.'], []),
    prolog_vars_to_json_vars(PrologTerm, JsonTerm),
    atom_json_term(Json, JsonTerm, [as(atom)]),
    write(Json).
	
test2 :-							   
		JSON = ' {
				  "Forall": {
					"Var":"V1",
					"Implies": {
					  "head": {"Rel":"entity", "Ind":"student", "Var":"V1"},
					  "body": {
						"Var":"V2",
						"Neg": {
						  "And": [
							{
							  "Atom": [
								{"Rel":"relation", "Ind":"study", "Var": ["V1", "V2" ]},
								{"Rel":"entity", "Ind":"unit", "Var":"V2"}
							  ]
							},
							{
							  "Atom": [
								{"Rel":"relation", "Ind":"teach", "Var": ["V1", "V2" ]},
								{"Rel":"entity", "Ind":"unit", "Var":"V2"}
							  ]
							}
						  ]
						}
					  }
					}
				  }
				}',
  atom_json_term(JSON, JSONTerm, [as(atom)]),
  json_vars_to_prolog_vars(JSONTerm, PrologVars),
  s([type:const, sem:PrologVars], Sentence, []),!,
  nl, nl,
  writeq(Sentence).

