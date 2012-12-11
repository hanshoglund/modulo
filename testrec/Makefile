
all:	gen

gen:
	modulo -Mtestrec 	<testrec/A.module    >testrec/a.h;	\
	modulo -Mtestrec 	<testrec/B.module    >testrec/b.h;	\
	modulo -Mtestrec	<testrec/C.module    >testrec/c.h;
	modulo -Llisp -Mtestrec 	<testrec/A.module    >testrec/a.h;	\
	modulo -Llisp -Mtestrec 	<testrec/B.module    >testrec/b.h;	\
	modulo -Llisp -Mtestrec	<testrec/C.module    >testrec/c.h;

clean:
	rm testrec/*.h; \
	rm testrec/*.lisp;