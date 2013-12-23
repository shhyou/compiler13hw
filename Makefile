AOUT = parser

ifeq ($(findstring $(OS), Windows_NT), Windows_NT)

ifeq ($(findstring sh, $(SHELL)), sh)
export EXE := .exe
export RM := rm -f

else

export EXE := .exe
export RM := del /Q
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
