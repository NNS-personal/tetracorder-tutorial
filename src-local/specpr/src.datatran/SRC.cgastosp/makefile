SHELL=/bin/sh

SRCFIL=/usr/src/cmd/specpr

OBJ=		\
cgastosp.o


cgastosp:	$(OBJ)
	$(F77) $(FFLAGS) $(LDFLAGS) -o cgastosp $(OBJ) $(SP_LIB)/specpr.a \
			$(SP_LIB)/sputil.a $(LDLIBS)

cgastosp.o:		cgastosp.r
	$(SSPP) -r -$(SSPPFLAGS) cgastosp.r > cgastosp.x
	$(RF) $(RFLAGS) cgastosp.x > cgastosp.f
	$(F77) $(FFLAGS) -c cgastosp.f
	rm -f cgastosp.x cgastosp.f

install:
	mv cgastosp $(SP_LOCAL)/bin/cgastosp
	chmod 755 $(SP_LOCAL)/bin/cgastosp
