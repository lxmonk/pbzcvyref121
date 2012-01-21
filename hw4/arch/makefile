DEP_LIST = char.lib cisc.h io.lib lib math.lib scheme.lib string.lib system.lib



all: genComp.c $(DEP_LIST)
	mkdir -p build
	gcc -ggdb genComp.c -o build/genComp
	echo 'compilation of generated file (from C) done.'
	(cd build; ./genComp)

man: compiler.c $(DEP_LIST)
	mkdir -p build
	gcc -ggdb compiler.c -o build/compiler
	echo 'compilation (from C) done.'
	(cd build; ./compiler inFile.scm)
