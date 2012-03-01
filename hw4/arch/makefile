DEP_LIST = char.lib cisc.h io.lib lib math.lib scheme.lib string.lib system.lib



all: genComp.c $(DEP_LIST)
	@echo 'running "make" (or "make all") will also execute the compiled code.'
	@echo 'if this is not desirable, use "make myGreatCode"'
	@echo
	@mkdir -p build
	gcc -ggdb genCode.c -o build/genCode
	@echo 'compilation of generated file (from C) done.'
	(cd build; ./genCode)
	@echo 'done.'

myGreatCode: genComp.c $(DEP_LIST)
	@mkdir -p build
	gcc -ggdb genCode.c -o build/genCode

compile: genComp.c $(DEP_LIST)
	@mkdir -p build
	gcc -ggdb genCode.c -o build/genCode
