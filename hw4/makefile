DEP_LIST = arch/char.lib arch/cisc.h arch/io.lib arch/lib arch/math.lib arch/scheme.lib arch/string.lib arch/system.lib

all: compiler.c $(DEP_LIST)
	mkdir -p build
	gcc -ggdb compiler.c -o build/compiler
	build/compiler infile.scm
