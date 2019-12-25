:- use_module(library(http/json)).
:- style_check([-discontiguous, -singleton]).
:- consult(predicates).
   
s([type:const, sem:Sem]) -->               
  np([num:N, type:const, pos:subj, arg:X, sco:[S1,S2], sem:Sem]),
  rc([num:N, type:const, arg:X, sem:[S1,S2]]).
  
rc([num:N, type:const, arg:X, sem:[S1,S2]]) --> 	
  prn([wfrm:[who]]),  
  vp([num:N, type:const, arg:X, sem:[S1,S2]]), ['.'].
    
np([num:N, type:const, pos:subj, arg:X, sco:[S1,S2], sem:Sem]) -->                 
  %qnt([num:N, type:const, arg:X, res:json(['And'=json(['Atom'=[R,S1]])]), sco:S2, sem:Sem]), 
  qnt([num:N, type:const, arg:X, res:json(['Implies'= json([head=R, body=S1])]), sco:S2, sem:Sem]),
  noun([num:N, type:const, pos:subj, arg:X, sem:R]).                          
    
np([num:_N, type:const, pos:obj, arg:X, sco:S, sem:Sem]) -->       
  cst([num:N, type:const, arg:X, res:R, sco:S, sem:Sem]),
  noun([num:N, type:const, pos:obj, arg:X, sem:R]).                          

vp([num:N, type:const, arg:X, sem:[S1,S3]]) -->
  verb([num:N, type:const, arg:X, arg:Y, sem:S]),
  np([num:_N, type:const, pos:obj, arg:Y, sco:S, sem:S1]),
  verb([num:N, type:const, arg:X, arg:Z, sem:S2]),
  np([num:_N, type:const, pos:obj, arg:Z, sco:S2, sem:S3]).

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

prn([wfrm:[who]]) --> [who].

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
									   
lexicon([cat:cst, wform:[1, or, more], num:pl, arg:X, res:Res, sco:Sco,
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
             
lexicon([cat:noun, wform:[department], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 

lexicon([cat:noun, wform:[program], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=program, 'Var'=X])]). 

lexicon([cat:noun, wform:[unit], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=unit, 'Var'=X])]). 

lexicon([cat:noun, wform:[units], num:pl, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=unit, 'Var'=X])]). 

lexicon([cat:noun, wform:[departments], num:pl, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 
                         
lexicon([cat:verb, wform:[belongs, to], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=belong_to, 'Var'=[X, Y]]) ]).  

lexicon([cat:verb, wform:[is, enrolled, in], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=enrolled_in, 'Var'=[X, Y]]) ]).  

lexicon([cat:verb, wform:[studies], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=study, 'Var'=[X, Y]]) ]).  


test1 :-
    s([type:const, sem:PrologTerm],
      ['Every', student, who, is, enrolled, in, exactly, 1, program, studies, 1, or, more, units, '.'], []),
    prolog_vars_to_json_vars(PrologTerm, JsonTerm),
    atom_json_term(Json, JsonTerm, [as(atom)]),
    write(Json).
	
test2 :-							   
		JSON = ' {
			  "Forall": {
				"Var":"V1",
				"Implies": {
				  "head": {
					"Implies": {
					  "head": {"Rel":"entity", "Ind":"student", "Var":"V1"},
					  "body": {
						"Exists": {
						  "Var":"V2",
						  "And": {
							"Atom": [
							  {
								"Rel":"relation",
								"Ind":"enrolled_in",
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
							  {"Rel":"entity", "Ind":"program", "Var":"V2"}
							]
						  }
						}
					  }
					}
				  },
				  "body": {
					"Exists": {
					  "Var":"V3",
					  "And": {
						"Atom": [
						  {"Rel":"relation", "Ind":"study", "Var": ["V1", "V3" ]},
						  {
							"Rel":"min",
							"Var":"V3",
							"Data": {"_type":"xs:integer", "__text":"1"}
						  },
						  {
							"Rel":"max",
							"Var":"V3",
							"Data": {"_type":"xs:string", "__text":"unbound"}
						  },
						  {"Rel":"entity", "Ind":"unit", "Var":"V3"}
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

