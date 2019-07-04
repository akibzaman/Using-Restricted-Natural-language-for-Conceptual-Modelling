specification(1,
[
['Student',is,an,entity,'.'],
['Program',is,an,entity,'.'],
['Unit',is,an,entity,'.'],
['Student',id,is,of,integer,data,type,'.'],
['Student',name,is,of,string,data,type,'.'],
['No',student,is,a,program,'.'],
['No',student,is,a,unit,'.'],
['No',unit,is,a,program,'.'],
['Every',student,is,enrolled,in,exactly,1,program,'.'],
['Every',program,is,composed,of,some,units,'.'],
['Every',unit,is,studied,by,some,students,'.'],
['Every',unit,is,belongs,to,some,programs,'.'],
['Every',student,possesses,a,student,id,and,possesses,a,student,name,'.'],
['Every',program,possesses,a,program,id,and,possesses,a,program,name,'.'],
['Every',unit,possesses,a,unit,code,and,possesses,a,unit,name,'.'],
['Every',student,studies,at,least,1,and,at,most,4,units,'.']]).

specification(2,
[
['Student',is,an,entity,'.'],
['Program',is,an,entity,'.'],
['Unit',is,an,entity,'.'],
['Enrollment',is,an,entity,'.'],
['Student',id,is,of,integer,data,type,'.'],
['Student',name,is,of,string,data,type,'.'],
['Student', is, enrolled, in, program, '.'],
['Program', is, composed, of, unit, '.'],
['Unit', is, belong, to, program, '.'],
['Enrollment', objectifies, student, is, enrolled, in, program,'.']]).


