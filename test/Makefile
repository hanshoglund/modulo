
all:
	modulo --language=lisp <test/Std.m  >test/Std.lisp; 	\
	modulo --language=lisp <test/Foo.m  >test/Foo.lisp; 	\
	modulo --language=lisp <test/List.m >test/List.lisp;    \
	modulo --language=c    <test/Std.m  >test/Std.h;        \
	modulo --language=c    <test/Foo.m  >test/Foo.h;        \
	modulo --language=c    <test/List.m >test/List.h;       \
	cclint -I test test/Std.h;                              \
	cclint -I test test/Foo.h;                              \
	cclint -I test test/List.h;

doc:
	doxygen test/Doxyfile

clean:
	rm test/*.h; \
	rm test/*.lisp;