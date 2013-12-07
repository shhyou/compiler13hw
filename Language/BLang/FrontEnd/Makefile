TARGET := ParserHappy.hs

all: $(TARGET)
clean:
	-$(RM) $(TARGET) *.hi *.o *.info

%.o: %.hs
	ghc -c -o $@ $<

ParserHappy.hs: Parser.y
#	happy -a -g -d -i -o $@ $<
	happy -a -g -i -o $@ $<
