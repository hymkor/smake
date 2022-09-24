ifeq ($(OS),Windows_NT)
    EXE=.exe
    RM=del
else
    EXE=
    RM=rm
endif
AOUT=$(notdir $(abspath .))$(EXE)
OFILES=$(subst .c,.o,$(wildcard *.c))

$(AOUT) : $(OFILES)
	gcc -o $@ $^

.c.o:
	gcc -c $<

clean:
	$(RM) $(AOUT) $(OFILES)
