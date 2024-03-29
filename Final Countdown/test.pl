specification(1,
[
['Student is an entity type.'],
['Department is an entity type.'],
['Course is an entity type.'],
['Teacher is an entity type.'],
['Enrolment is an entity type.'],
['Section is an entity type.'],
['Student id is of integer data type.'],
['Student name is of string data type.'],
['Department number is of integer data type.'],
['Department name is of string data type.'],
['Teacher id is of integer data type.'],
['Teacher name is of string data type.'],
['Course id is of integer data type.'],
['Course name is of string data type.'],
['Enrolment semester is of integer data type.'],
['Enrolment grade is of string data type.'],
['Section id is of integer data type.'],
['Section name is of string data type.'],
['Student belongs to department.'],
['Department contains student.'],
['Teacher works in department.'],
['Department employs teacher.'],
['Department offers course.'],
['Course is offered by department.'],
['Course is offered in sections.'],
['Teacher teaches student in course.'],
['Enrolment associates "Teacher teaches student in course".'],
['Every student belongs to exactly 1 department.'],
['Every department contains 1 or more students.'],
['Every teacher works in exactly 1 department.'],
['Every department contains 1 or more teachers.'],
['Every course is offered by exactly 1 department.'],
['Every department offers 1 or more courses.'],
['Every enrolment includes exactly 1 teacher.'],
['Every enrolment includes exactly 1 student.'],
['Every enrolment includes exactly 1 course.'],
['Every student owns exactly 1 student id and owns exactly 1 student name.'],
['Every teacher owns exactly 1 teacher id and owns exactly 1 teacher name.'],
['Every course owns exactly 1 course id and owns exactly 1 course name.'],
['Every department owns exactly 1 department number and owns exactly 1 department name.'],
['Every enrolment owns exactly 1 enrolment semester and owns exactly 1 enrolment grade.'],
['Every section owns exactly 1 section id and owns exactly 1 section name.'],
['Every section is dependent of exactly 1 course.']
]).


specification(2,
[
['Album is a class type.'],
['Artist is a class type.'],
['Track is a class type.'],
['Audio player is a class type.'],
['Play detail is a class type.'],
['Rock artist is a class type.'],
['Folk artist is a class type.'],
['Album id is of integer data type.'],
['Album name is of string data type.'],
['Artist id is of integer data type.'],
['Artist name is of string data type.'],
['Track id is of integer data type.'],
['Track name is of string data type.'],
['Track duration is of integer data type.'],
['Course name is of string data type.'],
['Audio player id is of integer data type.'],
['Play detail date is of date data type.'],
['Play detail time is of integer data type.'],
['Artist makes albums.'],
['Album is made by artist.'],
['Album contains tracks.'],
['Track belongs to album.'],
['Audio player plays track.'],
['Track is played by audio player.'],
['Play detail associates "Audio player plays track".'],
['Every artist makes 1 or more albums.'],
['Every album is made by exactly 1 artist.'],
['Every album contains 1 or more tracks.'],
['Every track belongs to exactly 1 album.'],
['Every play detail includes exactly 1 track.'],
['Every play detail includes exactly 1 audio player.'],
['Every album owns exactly 1 album id and owns exactly 1 album name.'],
['Every track owns exactly 1 track id and owns exactly 1 track name and owns exactly 1 track duration.'],
['Every artist owns exactly 1 artist id and owns exactly 1 artist name.'],
['Every play detail owns exactly 1 play detail date and owns exactly 1 play detail time.'],
['Every folk artist is an artist.'],
['Every folk artist is an artist.']
]).


specification(3,
[
['Student is an entity type.'],
['Program is an entity type.'],
['Course is an entity type.'],
['Graduate student is an entity type.'],
['Undergraduate student is an entity type.'],
['Registration is an entity type.'],
['Course offering is an entity type.'],
['Tutorship is an entity type.'],
['Student id is of integer data type.'],
['Student name is of string data type.'],
['Program id is of integer data type.'],
['Program name is of string data type.'],
['Course id is of integer data type.'],
['Course name is of string data type.'],
['Registration semester is of integer data type.'],
['Registration grade is of string data type.'],
['Registration year is of integer data type.'],
['Course offering year is of integer data type.'],
['Course offering semester is of integer data type.'],
['Student is enrolled in program.'],
['Program is enrolled by student.'],
['Student registers for course.'],
['Student studies course.'],
['Course is studied by student.'],
['Graduate student teaches course.'],
['Program offers course.'],
['Registration objectifies "Student registers for course".'],
['Course offering objectifies "Program offers course".'],
['Tutorship objectifies "Graduate student teaches course".'],
['Every student is enrolled in exactly 1 program.'],
['Every program is enrolled by 1 or more students.'],
['Every course offering includes exactly 1 program.'],
['Every course offering includes exactly 1 course.'],
['Every registration includes exactly 1 student.'],
['Every registration includes exactly 1 course.'],
['Every tutorship includes exactly 1 graduate student.'],
['Every tutorship includes exactly 1 course.'],
['Every student studies 1 or more courses.'],
['Every course is studied by 1 or more students.'],
['Every student owns exactly 1 student id and owns exactly 1 student name.'],
['Every program owns exactly 1 program id and owns exactly 1 program name.'],
['Every course owns exactly 1 course id and owns exactly 1 course name.'],
['Every registration owns exactly 1 registration semester and owns exactly 1 registration year.'],
['Every registration owns exactly 1 registration grade.'],
['Every course offering owns exactly 1 course offering semester and owns exactly 1 course offering year.'],
['Every graduate student is a student.'],
['Every undergraduate student is a student.'],
['Each student is a graduate student or is an undergraduate student.'],
['No undergraduate student is a graduate student.'],
['No graduate student studies a course and teaches the course.'],
['Every student who registers for 1 or more courses studies 1 or more courses.']
]).




