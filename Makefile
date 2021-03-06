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
	make -C Language/BLang/BackEnd clean
	make -C Language/BLang/CodeGen clean
	make -C Language/BLang/Debug clean

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
