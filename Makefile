AOUT = parser

ifeq ($(findstring $(OS), Windows_NT), Windows_NT)

export EXE := .exe

unam := $(shell uname 2>&1)
HOST_OS := unknown
ifeq ($(findstring MINGW, $(unam)), MINGW)
HOST_OS := mingw
endif
ifeq ($(findstring CYGWIN, $(unam)), CYGWIN)
HOST_OS := cygwin
endif
ifeq ($(findstring MSYS, $(unam)), MSYS)
HOST_OS := msys
endif

ifeq ($(findstring $(HOST_OS), cygwin mingw msys),)
export RM := del /Q
else
export RM := rm -f
endif

else
export EXE :=
export RM := rm -f
endif

.PHONY: all
all: compiler

.PHONY: clean
clean:
	-$(RM) *.hi *.o $(AOUT)$(EXE) Main.stamp
	make -C Language/BLang clean
	make -C Language/BLang/FrontEnd clean
	make -C Language/BLang/Semantic clean
	make -C Language/BLang/CodeGen clean
	make -C Language/BLang/Debug clean
	make -C Language/BLang/Homework clean

.PHONY: frontend
frontend:
	make -C Language/BLang/FrontEnd

.PHONY: compiler
compiler: frontend $(AOUT)$(EXE)

$(AOUT)$(EXE): Main.stamp
	ghc --make -O -o $@ Main.hs
	@$(RM) Main.stamp

Main.stamp:
	echo>Main.stamp

.PHONY: test1 test2 test3 test4 test5 test6 testa testb testc testd teste
test1: compiler
	@echo "int main() { int a[10], i; for (i = 0; i < 10; i=i+1) a[i] = i * i; return 0; }"
	@echo "int main() { int a[10], i; for (i = 0; i < 10; i=i+1) a[i] = i * i; return 0; }" | parser

test2: compiler
	@echo "int main() { int a = 2, b = 3 + 5; return (a+b) - (a-b); }"
	@echo "int main() { int a = 2, b = 3 + 5; return (a+b) - (a-b); }" | parser

test3: compiler
	@echo "int main() { float a = 2.0, b = 3.0 + 5.0; return (a+b) - (a-b); }"
	@echo "int main() { float a = 2.0, b = 3.0 + 5.0; return (a+b) - (a-b); }" | parser

test4: compiler
	@echo "int main() { float a = 3.14; if (4.0 > a && a > 2.71) return 3; else return -2; }"
	@echo "int main() { float a = 3.14; if (4.0 > a && a > 2.71) return 3; else return -2; }" | parser

test5: compiler
	@echo "int main() { write(\"Hello World!\n\"); return 0; }"
	@echo "int main() { write(\"Hello World!\n\"); return 0; }" | parser

test6: compiler
	@echo "int f() { return 0; } int main() { int a = 1, b = 8; return a + b + f(); }"
	@echo "int f() { return 0; } int main() { int a = 1, b = 8; return a + b + f(); }" | parser

testa: compiler
	@echo "assign.c"
	@parser < test/hw5/assign.c

testb: compiler
	@echo "control.c"
	@parser < test/hw5/control.c

testc: compiler
	@echo "expr.c"
	@parser < test/hw5/expr.c

testd: compiler
	@echo "func.c"
	@parser < test/hw5/func.c

teste: compiler
	@echo "io.c"
	@parser < test/hw5/io.c
