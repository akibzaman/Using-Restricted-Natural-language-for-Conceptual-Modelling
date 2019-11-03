:- use_module(library(http/json)).
:- style_check([-discontiguous, -singleton]).
:- consult(predicates).
   
s([type:const, sem:Sem]) -->               
  np([num:N, type:const, func:subj, arg:X, sco:S, sem:Sem]),
  vp([num:N, type:const, arg:X, sem:S]), ['.'].
    
np([num:N, type:const, func:subj, arg:X, sco:S, sem:Sem]) -->                 
  qnt([num:N, type:const, arg:X, res:json(['Atom'=R]), sco:S, sem:Sem]),
  noun([num:N, type:const, arg:X, sem:R]).                          
    
np([num:_N, type:const, func:obj, arg:X, sco:S, sem:Sem]) -->       
  cst([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]),
  noun([num:N, type:const, arg:X, sem:R]).                          

vp([num:N, type:const, arg:X, sem:Sem]) -->
  verb([num:N, type:const, arg:X, arg:Y, sem:S]),
  np([num:_N, type:const, func:obj, arg:Y, sco:S, sem:Sem]).

qnt([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 
  { lexicon([cat:qnt, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.
	
cst([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]) --> 					
  { lexicon([cat:cst, wform:WForm, num:N, arg:X, res:R, sco:S, sem:Sem]) }, WForm.

noun([num:N, type:const, arg:X, sem:Sem]) -->
  { lexicon([cat:noun, wform:WForm, num:N, type:entity, stype:const_dec, pos:_, arg:X, sem:Sem]) }, WForm.

verb([num:N, type:const, arg:X, arg:Y, sem:Sem]) -->
  { lexicon([cat:verb, wform:WForm, num:N, arg:X, arg:Y, sem:Sem]) }, WForm.


lexicon([cat:qnt, wform:['Every'], num:sg, arg:X, res:JsonHead, sco:JsonBody,
         sem:json(['Forall'=json(['Var'=X, 
                                   'Implies'= json([head=JsonHead,
                                                    body=JsonBody])])]) ]).

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
									   

lexicon([cat:noun, wform:[student], num:sg, type:entity, stype:const_dec, pos:_, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   
             
lexicon([cat:noun, wform:[department], num:sg, type:entity, stype:const_dec, pos:_, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 

lexicon([cat:noun, wform:[departments], num:pl, type:entity, stype:const_dec, pos:_, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 
                         
lexicon([cat:verb, wform:[belongs, to], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=belong_to, 'Var'=[X, Y]]) ]).  


test1 :-
    s([type:const, sem:PrologTerm],
      ['Every', student, belongs, to, exactly, 1, department, '.'], []),
    prolog_vars_to_json_vars(PrologTerm, JsonTerm),
    atom_json_term(Json, JsonTerm, [as(atom)]),
    write(Json).
	
test2 :-							   
	JSON = ' {
				  "Forall": {
					  "Var":"V1",
					  "Implies": {
						"head": {"Atom": {"Rel":"entity", "Ind":"student", "Var":"V1"}},
						"body": {
						  "Exists": {
							"Var":"V2",
							"And": {
							  "Atom": [
								{
								  "Rel":"relation",
								  "Ind":"belong_to",
								  "Var": ["V1", "V2" ]
								},
								{
								  "Rel":"min",
								  "Var":"V2",
								  "Data": {"_type":"xs:integer", "__text":"1"}
								},
								{
								  "Rel":"max",
								  "Var":"V2",
								  "Data": {"_type":"xs:integer", "__text":"1"}
								},
								{"Rel":"entity", "Ind":"department", "Var":"V2"}
							  ]
							}
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

test3 :-
    s([type:const, sem:PrologTerm],
      ['Every', student, belongs, to, at, least, 1, and, at, most, 4, departments, '.'], []),
    prolog_vars_to_json_vars(PrologTerm, JsonTerm),
    atom_json_term(Json, JsonTerm, [as(atom)]),
    nl, nl,
    write(Json).
  
test4 :-							   
	JSON = ' {
			  "Forall": {
				"Var":"V1",
				"Implies": {
				  "head": {"Atom": {"Rel":"entity", "Ind":"student", "Var":"V1"}},
				  "body": {
					"Exists": {
					  "Var":"V2",
					  "And": {
						"Atom": [
						  {
							"Rel":"relation",
							"Ind":"belong_to",
							"Var": ["V1", "V2" ]
						  },
						  {
							"Rel":"min",
							"Var":"V2",
							"Data": {"_type":"xs:integer", "__text":1}
						  },
						  {
							"Rel":"max",
							"Var":"V2",
							"Data": {"_type":"xs:integer", "__text":4}
						  },
						  {"Rel":"entity", "Ind":"department", "Var":"V2"}
						]
					  }
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
