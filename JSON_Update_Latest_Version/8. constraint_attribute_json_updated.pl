:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
:- consult(predicates). 

s([type:const_dp, sem:Sem]) -->
   np([num:N, type:const_dp, pos:subj, arg:X, sco:Sco, sem:Sem]),
   vp([crd:'+', num:N, type:const_dp, arg:X, sem:Sco]),
   ['.'].

np([num:N, type:const_dp, pos:P, arg:X, sco:Sco, sem:Sem]) -->
   qnt([num:N, type:const_dp, pos:P, arg:X, res:json(['Atom'=Res]), sco:Sco, sem:Sem]),
   noun([num:sg, type:const_dp, pos:P, arg:X, sem:Res]).

np([num:N, type:const_dp, pos:P, arg:Y, sco:Sco, sem:Sem]) -->
   cst([num:N, type:const_dp, pos:P, arg:Y, res:Res, sco:Sco, sem:Sem]),
   noun([num:N, type:const_dp, pos:P, arg:Y, sem:Res]).

noun([num:N, type:const_dp, pos:subj, arg:X, sem:Sem]) -->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, stype:const_dec, pos:subj, arg:X, sem:Sem])}, WForm.
  
noun([num:N, type:const_dp, pos:obj, arg:Y, sem:Sem]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:attribute, pos:obj, dt:DT, arg:Y, sem:Sem])}, WForm.
   

vp([crd:'+', num:N, type:const_dp, arg:X, sem:json(['Exists'=[S1, S2]])]) -->   
    vp([crd:'-', num:N, type:const_dp, arg:X, sem:S1]),
    cc([wfm:[and]]),
    vp([crd:'+', num:N, type:const_dp, arg:X, sem:S2]).


vp([crd:'+', num:N, type:const_dp, arg:X, sem:Sem]) -->   
    verb([num:N, type:const_dp, arg:X, arg:Y, sem:Sco]), 
    np([num:_, type:const_dp, pos:obj, arg:Y, sco:Sco, sem:Sem]).


vp([crd:'-', num:N, type:const_dp, arg:X, sem:Sem]) -->   
    verb([num:N, type:const_dp, arg:X, arg:Y, sem:Sco]), 
    np([num:_, type:const_dp, pos:obj, arg:Y, sco:Sco, sem:Sem]).
   
verb([num:N, type:const_dp, arg:X, arg:Y, sem:Sem]) -->
   { lexicon([cat:verb, wform:WForm, num:N, arg:X, arg:Y, sem:Sem]) }, WForm.

qnt([num:N, type:const_dp, pos:subj, arg:X, res:JsonHead, sco:JsonBody, 
					sem:json(['Forall'=json(['Var'=X, 
						'Implies'= json([head=JsonHead, body=JsonBody])])])]) --> ['Every'].
   
cst([num:N, type:const_dp, pos:obj, arg:X, res:Res, sco:Sco, 
					sem:json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         Res ]
                                       ])])]) --> [exactly, 1].


cc([wfm:[and]]) --> [and].   

% ----------------------------------------------------------------------------------

lexicon([cat:noun, wform:[student], num:sg, type:entity, stype:const_dec, pos:_, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   

lexicon([cat:noun, wform:[student, id], num:sg, type:attribute, pos:obj, dt:integer, arg:X, 
		sem:json(['Rel'=attribute, 'Ind'=student_id, 'Var'=json(['Type'=integer, 'Text'=X])])]).

lexicon([cat:noun, wform:[student, name], num:sg, type:attribute, pos:obj, dt:string, arg:X, 
		sem:json(['Rel'=attribute, 'Ind'=student_name, 'Var'=json(['Type'=string, 'Text'=X])])]).


lexicon([cat:verb, wform:[owns], num:sg, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=own, 'Var'=[X, Y]])]).

% ----------------------------------------------------------------------------------

test1 :-
   s([type:const_dp, sem:PrologTerm], [Every, student, owns, exactly, 1, student, id, and, owns, exactly, 1, student, name, '.'], []),
   prolog_vars_to_json_vars(PrologTerm, JsonTerm), 
   atom_json_term(JSON, JsonTerm, [as(atom)]),
   write(JSON).

test2 :-							   
	JSON = ' {
			  "Forall": {
				"Var":"V1",
				"Implies": {
				  "head": {"Atom": {"Rel":"entity", "Ind":"student", "Var":"V1"}},
				  "body": {
					"Exists": [
					  {
						"Var":"V2",
						"And": {
						  "Atom": [
							{"Rel":"relation", "Ind":"own", "Var": ["V1", "V2" ]},
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
							{
							  "Rel":"attribute",
							  "Ind":"student_id",
							  "Var": {"Type":"integer", "Text":"V2"}
							}
						  ]
						}
					  },
					  {
						"Var":"V3",
						"And": {
						  "Atom": [
							{"Rel":"relation", "Ind":"own", "Var": ["V1", "V3" ]},
							{
							  "Rel":"min",
							  "Var":"V3",
							  "Data": {"_type":"xs:integer", "__text":"1"}
							},
							{
							  "Rel":"max",
							  "Var":"V3",
							  "Data": {"_type":"xs:integer", "__text":"1"}
							},
							{
							  "Rel":"attribute",
							  "Ind":"student_name",
							  "Var": {"Type":"string", "Text":"V3"}
							}
						  ]
						}
					  }
					]
				  }
				}
			  }
			}',
  atom_json_term(JSON, JSONTerm, [as(atom)]),
  json_vars_to_prolog_vars(JSONTerm, PrologVars),
  s([type:const_dp, sem:PrologVars], Sentence, []),!,
  nl, nl,
  writeq(Sentence).
