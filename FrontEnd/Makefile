TARGET := Parser.hs

all: $(TARGET)
clean:
	-$(RM) $(TARGET) 
	-$(RM) *.hi *.o *.info

%.o: %.hs
	ghc -c -o $@ $<

Parser.hs: Parser.y
	happy -i Parser.y
