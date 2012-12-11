
all:	gen

gen:
	modulo -Mtest --language=lisp <test/Std.module    >test/Std.lisp;	\
	modulo -Mtest --language=lisp <test/List.module   >test/List.lisp	\
	modulo -Mtest --language=lisp <test/Thread.module >test/Thread.h;	\
	modulo -Mtest --language=c    <test/Std.module    >test/Std.h;		\
	modulo -Mtest --language=c    <test/Foo.module    >test/Foo.h;		\
	modulo -Mtest --language=c    <test/List.module   >test/List.h;	\
	modulo -Mtest --language=c    <test/Thread.module >test/Thread.h;	\
	cclint -I test test/Std.h;					\
	cclint -I test test/Foo.h;					\
	cclint -I test test/List.h;					\
	cclint -I test test/Thread.h;

doc:    gen
	doxygen test/Doxyfile

clean:
	rm test/*.h; \
	rm test/*.lisp;