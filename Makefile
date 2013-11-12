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
	-$(RM) *.hi *.o
	-$(RM) $(AOUT)$(EXE)
	make -C Frontend clean

.PHONY: frontend
frontend:
	make -C Frontend

.PHONY: compiler
compiler: frontend $(AOUT)$(EXE)

$(AOUT)$(EXE): Main.hs
	ghc --make -o $@ $<
