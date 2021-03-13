all:
	yacc -o gpp_interpreter.c -d gpp_interpreter.y
	flex -o gpp_lexer.c gpp_interpreter.l
	gcc gpp_lexer.c gpp_interpreter.c -o gpp_interpreter.out -lm
clean:
	rm gpp_lexer.out