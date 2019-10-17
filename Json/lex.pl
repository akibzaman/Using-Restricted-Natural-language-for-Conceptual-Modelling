lexicon([cat:verb,wform:[associates],num:sg,type:ob_rel,sub:A,arg:B,arg:C,sem:json(['Atom'=[A,json(['Rel'=relation,'Ind'=associates,'Var'=B,'Reify'=C])]])]).
lexicon([cat:verb,wform:[includes],num:sg,type:brel,arg:A,arg:B,sem:json(['Rel'=relation,'Ind'=include,'Var'=[A,B]])]).
lexicon([cat:verb,wform:[is,dependent,of],num:sg,type:brel,arg:A,arg:B,sem:json(['Rel'=relation,'Ind'=dependent_of,'Var'=[A,B]])]).
lexicon([cat:qnt,wfm:['Every'],num:sg,arg:A,res:B,sco:C,sem:json(['Forall'=json(['Var'=A,'Implies'=json([head=B,body=C])])])]).
lexicon([cat:cst,wfm:[exactly,1],num:sg,arg:A,res:B,sco:C,sem:json(['Exists'=json(['Var'=A,'And'=json(['Atom'=[C,json(['Rel'=min,'Var'=A,'Data'=json(['_type'='xs:integer','__text'='1'])]),json(['Rel'=max,'Var'=A,'Data'=json(['_type'='xs:integer','__text'='1'])]),B]])])])]).
lexicon([cat:cst,wfm:[at,least,A,and,at,most,B],num:sg,arg:C,res:D,sco:E,sem:json(['Exists'=json(['Var'=C,'And'=json(['Atom'=[E,json(['Rel'=min,'Var'=C,'Data'=json(['_type'='xs:integer','__text'=A])]),json(['Rel'=max,'Var'=C,'Data'=json(['_type'='xs:integer','__text'=B])]),D]])])])]).
lexicon([cat:cst,wfm:[1,or,more],num:sg,arg:A,res:B,sco:C,sem:json(['Exists'=json(['Var'=A,'And'=json(['Atom'=[C,json(['Rel'=min,'Var'=A,'Data'=json(['_type'='xs:integer','__text'='1'])]),json(['Rel'=max,'Var'=A,'Data'=json(['_type'='xs:string','__text'=unbound])]),B]])])])]).
lexicon([cat:noun,wform:['Student'],num:sg,type:entity,pos:subj,arg:A,sem:json(['Rel'=entity,'Ind'=student,'Var'=A])]).
lexicon([cat:noun,wform:[student],num:sg,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=student,'Var'=A])]).
lexicon([cat:noun,wform:[students],num:pl,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=student,'Var'=A])]).
lexicon([cat:noun,wform:['Department'],num:sg,type:entity,pos:subj,arg:A,sem:json(['Rel'=entity,'Ind'=department,'Var'=A])]).
lexicon([cat:noun,wform:[department],num:sg,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=department,'Var'=A])]).
lexicon([cat:noun,wform:[departments],num:pl,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=department,'Var'=A])]).
lexicon([cat:noun,wform:['Course'],num:sg,type:entity,pos:subj,arg:A,sem:json(['Rel'=entity,'Ind'=course,'Var'=A])]).
lexicon([cat:noun,wform:[course],num:sg,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=course,'Var'=A])]).
lexicon([cat:noun,wform:[courses],num:pl,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=course,'Var'=A])]).
lexicon([cat:noun,wform:['Teacher'],num:sg,type:entity,pos:subj,arg:A,sem:json(['Rel'=entity,'Ind'=teacher,'Var'=A])]).
lexicon([cat:noun,wform:[teacher],num:sg,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=teacher,'Var'=A])]).
lexicon([cat:noun,wform:[teachers],num:pl,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=teacher,'Var'=A])]).
lexicon([cat:noun,wform:['Enrolment'],num:sg,type:entity,pos:subj,arg:A,sem:json(['Rel'=entity,'Ind'=enrolment,'Var'=A])]).
lexicon([cat:noun,wform:[enrolment],num:sg,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=enrolment,'Var'=A])]).
lexicon([cat:noun,wform:[enrolments],num:pl,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=enrolment,'Var'=A])]).
lexicon([cat:noun,wform:['Section'],num:sg,type:entity,pos:subj,arg:A,sem:json(['Rel'=entity,'Ind'=section,'Var'=A])]).
lexicon([cat:noun,wform:[section],num:sg,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=section,'Var'=A])]).
lexicon([cat:noun,wform:[sections],num:pl,type:entity,pos:obj,arg:A,sem:json(['Rel'=entity,'Ind'=section,'Var'=A])]).
lexicon([cat:noun,wform:['Student',id],num:sg,type:attribute,dt:integer,arg:A,sem:json(['Rel'=attribute,'Ind'=student_id,'Var'=json(['Type'=integer,'Text'=A])])]).
lexicon([cat:noun,wform:['Student',name],num:sg,type:attribute,dt:string,arg:A,sem:json(['Rel'=attribute,'Ind'=student_name,'Var'=json(['Type'=string,'Text'=A])])]).
lexicon([cat:noun,wform:['Department',number],num:sg,type:attribute,dt:integer,arg:A,sem:json(['Rel'=attribute,'Ind'=department_number,'Var'=json(['Type'=integer,'Text'=A])])]).
lexicon([cat:noun,wform:['Department',name],num:sg,type:attribute,dt:string,arg:A,sem:json(['Rel'=attribute,'Ind'=department_name,'Var'=json(['Type'=string,'Text'=A])])]).
lexicon([cat:noun,wform:['Teacher',id],num:sg,type:attribute,dt:integer,arg:A,sem:json(['Rel'=attribute,'Ind'=teacher_id,'Var'=json(['Type'=integer,'Text'=A])])]).
lexicon([cat:noun,wform:['Teacher',name],num:sg,type:attribute,dt:string,arg:A,sem:json(['Rel'=attribute,'Ind'=teacher_name,'Var'=json(['Type'=string,'Text'=A])])]).
lexicon([cat:noun,wform:['Course',id],num:sg,type:attribute,dt:integer,arg:A,sem:json(['Rel'=attribute,'Ind'=course_id,'Var'=json(['Type'=integer,'Text'=A])])]).
lexicon([cat:noun,wform:['Course',name],num:sg,type:attribute,dt:string,arg:A,sem:json(['Rel'=attribute,'Ind'=course_name,'Var'=json(['Type'=string,'Text'=A])])]).
lexicon([cat:noun,wform:['Enrolment',semester],num:sg,type:attribute,dt:integer,arg:A,sem:json(['Rel'=attribute,'Ind'=enrolment_semester,'Var'=json(['Type'=integer,'Text'=A])])]).
lexicon([cat:noun,wform:['Enrolment',grade],num:sg,type:attribute,dt:string,arg:A,sem:json(['Rel'=attribute,'Ind'=enrolment_grade,'Var'=json(['Type'=string,'Text'=A])])]).
lexicon([cat:noun,wform:['Section',id],num:sg,type:attribute,dt:integer,arg:A,sem:json(['Rel'=attribute,'Ind'=section_id,'Var'=json(['Type'=integer,'Text'=A])])]).
lexicon([cat:noun,wform:['Section',name],num:sg,type:attribute,dt:string,arg:A,sem:json(['Rel'=attribute,'Ind'=section_name,'Var'=json(['Type'=string,'Text'=A])])]).
lexicon([cat:verb,wform:[belongs,to],num:sg,type:brel,arg:A,arg:B,sem:json(['Rel'=relation,'Ind'=belong_to,'Var'=[A,B]])]).
lexicon([cat:verb,wform:[contains],num:sg,type:brel,arg:A,arg:B,sem:json(['Rel'=relation,'Ind'=contain,'Var'=[A,B]])]).
lexicon([cat:verb,wform:[works,in],num:sg,type:brel,arg:A,arg:B,sem:json(['Rel'=relation,'Ind'=work_in,'Var'=[A,B]])]).
lexicon([cat:verb,wform:[employs],num:sg,type:brel,arg:A,arg:B,sem:json(['Rel'=relation,'Ind'=emploi,'Var'=[A,B]])]).
lexicon([cat:verb,wform:[offers],num:sg,type:brel,arg:A,arg:B,sem:json(['Rel'=relation,'Ind'=offer,'Var'=[A,B]])]).
lexicon([cat:verb,wform:[is,offered,by],num:sg,type:brel,arg:A,arg:B,sem:json(['Rel'=relation,'Ind'=offered_by,'Var'=[A,B]])]).
lexicon([cat:verb,wform:[is,offered,in],num:sg,type:brel,arg:A,arg:B,sem:json(['Rel'=relation,'Ind'=offered_in,'Var'=[A,B]])]).
lexicon([cat:verb,wform:[[teaches],[in]],num:sg,type:trel,arg:A,label:teach,arg:B,label:in,arg:C,sem:json(['Rel'=relation,'Ind'=teach__in,'Var'=[A,B,C]])]).
