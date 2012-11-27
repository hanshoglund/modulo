
all:	gen

gen:
	modulo --language=lisp <test/Std.module    >test/Std.lisp;	\
	modulo --language=lisp <test/Foo.module    >test/Foo.lisp;	\
	modulo --language=lisp <test/List.module   >test/List.lisp	\
	modulo --language=lisp <test/Thread.module >test/Thread.h;	\
	modulo --language=c    <test/Std.module    >test/Std.h;		\
	modulo --language=c    <test/Foo.module    >test/Foo.h;		\
	modulo --language=c    <test/List.module   >test/List.h;	\
	modulo --language=c    <test/Thread.module >test/Thread.h;	\
	cclint -I test test/Std.h;					\
	cclint -I test test/Foo.h;					\
	cclint -I test test/List.h;					\
	cclint -I test test/Thread.h;

doc:    gen
	doxygen test/Doxyfile

clean:
	rm test/*.h; \
	rm test/*.lisp;