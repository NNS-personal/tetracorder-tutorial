# %A%

SHELL=/bin/sh


# chkendian and byteorder can be located in src.specpr/syslib
# Putting here for now to get working.

OBJ=	\
chkendian.o \
byteorder.o \
specio.o \
io_specpr.o
PROG=fstospecpr

$(PROG):	$(OBJ)
	cc $(CFLAGS) -o $@ $(OBJ) -lm

chkendian.o:	chkendian.c
	cc $(CFLAGS) -c chkendian.c

byteorder.o:	byteorder.c
	cc $(CFLAGS) -c byteorder.c

specio.o: 	specio.c
	cc $(CFLAGS) -c specio.c

io_specpr.o: 	io_specpr.c
	cc $(CFLAGS) -c io_specpr.c

