:- style_check([-discontiguous, -singleton]).
:- use_module(library(http/json)).
  
:- op(900, yfx, '==>').
:- op(800, yfx, '&').
:- op(900, yfx, ':').
:- op(800, yfx, '#').  
:- consult(p_to_j). 

s([mode:M, type:const_dp, sem:Sem]) -->
   np([num:N, mode:M, type:const_dp, pos:obj, arg:X, scope:Sco, sem:Sem]),
   vp([crd:'+', num:N, mode:M, type:const_dp, arg:X, sem:Sco]),
   ['.'].

np([num:N, mode:M, type:const_dp, pos:P, arg:X, scope:Sco, sem:Sem]) -->
   qnt([num:N, mode:M, type:const_dp, pos:subj, arg:X, restrictor:json(['Atom'=Res]), scope:Sco, sem:Sem]),
   noun([num:sg, mode:M, type:const_dp, pos:P, arg:X, sem:Res]).
   
np([num:N, mode:M, type:const_dp, pos:P, arg:Y, scope:Sco, sem:Sem]) -->
   cst([num:N, mode:M, type:const_dp, pos:P, arg:Y, restrictor:Res, scope:Sco, sem:Sem]),
   noun([num:N, mode:M, type:const_dp, arg:Y, sem:Res]).
   
noun([num:N, mode:M, type:const_dp, pos:P, arg:X, sem:Res]) -->
   {lexicon([cat:noun, wform:WForm, num:N, type:entity, pos:P, arg:X, sem:Res])}, WForm.
  
noun([num:N, mode:M, type:const_dp, arg:Y, sem:Res]) -->
  {lexicon([cat:noun, wform:WForm, num:N, type:attribute, dt:DT, arg:Y, sem:Res])}, downcase_noun(WForm, W).
   

vp([crd:'+', num:N, mode:M, type:const_dp, arg:X, sem:json(['Exists'=[S1,S2]])]) -->   
    vp([crd:'-', num:N, mode:M, type:const_dp, arg:X, sem:S1]),
    cc([wfm:[and]]),
    vp([crd:'+', num:N, mode:M, type:const_dp, arg:X, sem:S2]).
   

vp([crd:'+', num:N, mode:M, type:const_dp, arg:X, sem:Sco]) -->   
    v([num:N, mode:M, type:const_dp, arg:X, arg:Y, sem:Sem]), 
    np([num:_, mode:M, type:const_dp, pos:obj, arg:Y, scope:Sem, sem:Sco]).


vp([crd:'-', num:N, mode:M, type:const_dp, arg:X, sem:Sco]) -->   
    v([num:N, mode:M, type:const_dp, arg:X, arg:Y, sem:Sem]), 
    np([num:_, mode:M, type:const_dp, pos:obj, arg:Y, scope:Sem, sem:Sco]).

v([num:N, mode:M, type:const_dp, arg:X, arg:Y, sem:json(['Rel'=relation, 'Ind'=own, 'Var'=[X,Y]])]) -->
   [own].

qnt([num:N, mode:M, type:const_dp, pos:subj, arg:X, restrictor:JsonHead, scope:JsonBody, 
					sem:json(['Forall'=json(['Var'=X, 
						'Implies'= json([head=JsonHead, body=JsonBody])])])]) --> ['Every'].
   
cst([num:N, mode:M, type:const_dp, pos:obj, arg:X, restrictor:Res, scope:Sco, 
					sem:json(['Var'=X,
                    'And'=json(['Atom'=[ Sco,
                                         json(['Rel'=min,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         json(['Rel'=max,
                                               'Var'=X,
                                               'Data'=json(['_type'='xs:integer','__text'='1'])]),
                                         Res ]
                                       ])])]) -->[exactly, 1].

cc([wfm:[and]]) --> [and].   

% ----------------------------------------------------------------------------------

lexicon([cat:noun, wform:[student], num:sg, type:entity, pos:obj, arg:X, sem:json(['Rel'=entity, 'Ind'=student, 'Var'=X])]).   
lexicon([cat:noun, wform:['Student', id], num:sg, type:attribute, dt:integer, arg:X, 
		sem:json(['Rel'=attribute, 'Ind'=student_id, 'Var'=json(['Type'=integer, 'Text'=X])])]).
lexicon([cat:noun, wform:['Student', name], num:sg, type:attribute, dt:string, arg:X, 
		sem:json(['Rel'=attribute, 'Ind'=student_name, 'Var'=json(['Type'=string, 'Text'=X])])]).

% ----------------------------------------------------------------------------------

test1 :-
   s([mode:proc, type:const_dp, sem:PrologTerm], [Every, student, own, exactly, 1, student, id, and, own, exactly, 1, student, name, '.'], []),
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
  s([mode:gen, type:const_dp, sem:JSONTerm], Sentence, []),
  nl, nl,
  write(Sentence).
