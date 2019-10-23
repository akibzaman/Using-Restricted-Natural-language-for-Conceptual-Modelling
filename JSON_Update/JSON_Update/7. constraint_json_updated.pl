:- use_module(library(http/json)).
:- style_check([-discontiguous, -singleton]).
:- consult(predicates).
   
s([type:const, sem:L]) -->               
  np([num:N, type:const, func:subj, arg:X, sco:S, sem:L]),
  vp([num:N, type:const, arg:X, sem:S]), ['.'].
    
np([num:N, type:const, func:subj, arg:X, sco:S, sem:L]) -->                 
   qnt([num:N, type:const, arg:X, res:json(['Atom'=R]), sco:S, sem:L]),
   noun([num:N, type:const, arg:X, sem:R]).                          
    
np([num:_N, type:const, func:obj, arg:X, sco:S, sem:L]) -->       
   cst([num:N, type:const, arg:X, res:R, sco:S, sem:L]),
   noun([num:N, type:const, arg:X, sem:R]).                          

vp([num:N, type:const, arg:X, sem:L]) -->
   verb([num:N, type:const, arg:X, arg:Y, sem:S]),
   np([num:_N, type:const, func:obj, arg:Y, sco:S, sem:L]).

qnt([num:N, type:const, arg:X, res:R, sco:S, sem:L0]) --> 
   { lexicon([cat:qnt, wfm:[Wfm|Wfms], num:N, arg:X, res:R, sco:S, sem:L0]) }, [Wfm|Wfms].
	
cst([num:N, type:const, arg:X, res:R, sco:S, sem:L0]) --> 					
   { lexicon([cat:cst, wfm:[Wfm|Wfms], num:N, arg:X, res:R, sco:S, sem:L0]) }, [Wfm|Wfms].

noun([num:N, type:const, arg:X, sem:L0]) -->
   { lexicon([cat:noun, wfm:[Wfm|Wfms], num:N, type:entity, pos:obj, arg:X, sem:L0]) }, [Wfm|Wfms].

verb([num:N, type:const, arg:X, arg:Y, sem:L0]) -->
   { lexicon([cat:verb, wfm:[Wfm|Wfms], num:N, arg:X, arg:Y, sem:L0]) }, [Wfm|Wfms].


lexicon([cat:qnt, wfm:['Every'], num:sg, arg:X, res:JsonHead, sco:JsonBody,
         sem:json(['Forall'=json(['Var'=X, 
                                   'Implies'= json([head=JsonHead,
                                                    body=JsonBody])])]) ]).

lexicon([cat:cst, wfm:[exactly, 1], num:sg, arg:X, res:Res, sco:Sco,
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


lexicon([cat:cst, wfm:[at, least, L, and, at, most, M], num:pl, arg:X, res:Res, sco:Sco,
         sem:json(['Exists'=json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'=L])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'=M])]),
                                         Res ]
                                       ])])]) ]).% :- atom_number(L1, L), atom_number(M1, M).
									   
lexicon([cat:cst, wfm:[1, or, more], num:sg, arg:X, res:Res, sco:Sco,
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
									   

lexicon([cat:noun, wfm:[student], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   
             
lexicon([cat:noun, wfm:[department], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 

lexicon([cat:noun, wfm:[departments], num:pl, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=department, 'Var'=X])]). 
                         
lexicon([cat:verb, wfm:[belongs, to], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=belong_to, 'Var'=[X, Y]]) ]).   

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
