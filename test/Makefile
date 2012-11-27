
all:	gen

gen:
	modulo --language=lisp <test/Std.m    >test/Std.lisp; 	\
	modulo --language=lisp <test/Foo.m    >test/Foo.lisp; 	\
	modulo --language=lisp <test/List.m   >test/List.lisp;  \
	modulo --language=lisp <test/Thread.m >test/Thread.h;   \
	modulo --language=c    <test/Std.m    >test/Std.h;      \
	modulo --language=c    <test/Foo.m    >test/Foo.h;      \
	modulo --language=c    <test/List.m   >test/List.h;     \
	modulo --language=c    <test/Thread.m >test/Thread.h;   \
	cclint -I test test/Std.h;                              \
	cclint -I test test/Foo.h;                              \
	cclint -I test test/List.h;				\
	cclint -I test test/Thread.h;

doc:    gen
	doxygen test/Doxyfile

clean:
	rm test/*.h; \
	rm test/*.lisp;