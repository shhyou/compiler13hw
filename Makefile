AOUT = parser

ifeq ($(findstring $(OS), Windows_NT), Windows_NT)
export EXE := .exe
export RM := del /Q
else
export EXE :=
export RM := rm -f
export 
endif

.PHONY: all
all: compiler

.PHONY: clean
clean:
	-$(RM) *.hi *.o $(AOUT)$(EXE) Main.stamp
	make -C Language/BLang/FrontEnd clean
	make -C Language/BLang/Debug clean

.PHONY: frontend
frontend:
	make -C Language/BLang/FrontEnd

.PHONY: debug
debug:
	make -C Language/BLang/Debug

.PHONY: compiler
compiler: frontend debug $(AOUT)$(EXE)

$(AOUT)$(EXE): Main.stamp
	ghc --make -o $@ Main.hs
	@$(RM) Main.stamp

Main.stamp:
	echo>Main.stamp
