SHELL=/bin/sh

OBJ=		\
asciitosp.o


asciitosp:	$(OBJ)
	$(F77) $(FFLAGS) $(LDFLAGS) -o asciitosp $(OBJ) $(SP_LIB)/specpr.a \
			$(SP_LIB)/sputil.a $(LDLIBS)

.SUFFIXES:
.SUFFIXES: .o .r


asciitosp.o:		asciitosp.r
	$(SSPP) -r -$(SSPPFLAGS)  asciitosp.r > asciitosp.x
	$(RF) $(RFLAGS) asciitosp.x > asciitosp.f
	$(F77) $(FFLAGS) -c asciitosp.f
	rm -f asciitosp.x asciitosp.f

install:
	mv asciitosp $(SP_LOCAL)/bin/asciitosp
	chmod 755 $(SP_LOCAL)/bin/asciitosp
