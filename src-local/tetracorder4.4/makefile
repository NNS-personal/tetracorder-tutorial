GET= get
SPRDIR=../specpr/

OBJ = tetracorder.o	\
makvicarlabel.o		\
parsevicarlabel.o		\
parseenvilabel.o		\
wrtenvihdr.o		\
str2tmonth.o            \
read2sheet.o		\
setupreadsheet.o		\
rstbgn.o			\
strdate.o			\
delhist.o			\
tri1setup.o			\
reflsetup.o			\
cubecorder.o		\
specorder.o			\
tp1mat.o			\
tp1all.o			\
flushseqfile.o		\
opencube.o			\
wrtspcrdrout.o		\
wrtcrdrout.o		\
gcsetup.o			\
wrtgcsetup.o		\
creatoutfiles.o		\
cublineout.o		\
tp1cse.o			\
nvres1mat.o			\
i4pack4.o			\
../specpr/src.specpr/common/spblockdata.o

INCL= \
../specpr/src.specpr/common/label1 \
../specpr/src.specpr/common/lbl3 \
../specpr/src.specpr/common/lbl4 \
../specpr/src.specpr/common/lbl7 \
../specpr/src.specpr/common/lundefs \
../specpr/src.specpr/common/alphabet \
../specpr/src.specpr/common/cmd \
../specpr/src.specpr/common/lblg \
../specpr/src.specpr/common/lblwav \
../specpr/src.specpr/common/cmdarg \
../specpr/src.specpr/common/dscrch \
../specpr/src.specpr/common/ioftyp \
../specpr/src.specpr/common/blank \
../specpr/src.specpr/common/lblvol \
tricube.h	\
tri1.h	\
obuffers.h	\
multmap.h

tetracorder:	$(OBJ) $(SPRDIR)/lib/specpr.a
	$(F77) $(SP_FFLAGS) $(SP_LDFLAGS) -o tetracorder $(OBJ) \
			$(SPRDIR)/lib/specpr.a	\
			$(SP_LDLIBS)

install:
	mv tetracorder /usr/local/bin/tetracorder4.4
	chmod 755 /usr/local/bin/tetracorder4.4
	size /usr/local/bin/tetracorder4.4

.SUFFIXES:
.SUFFIXES: .o .r .f


tetracorder.o:	tetracorder.r $(INCL) tricube.h tri1.h multmap.h obuffers.h
		$(SSPP) -r -$(SSPPFLAGS) tetracorder.r > tetracorder.x
		$(RF) $(SP_RFLAGS) tetracorder.x > tetracorder.f
		rm tetracorder.x
		$(F77) $(SP_FFLAGS) -c tetracorder.f
		rm tetracorder.f

makvicarlabel.o:	makvicarlabel.r
		$(RF) $(SP_RFLAGS) makvicarlabel.r > makvicarlabel.f
		$(F77) $(SP_FFLAGS) -c makvicarlabel.f
		rm makvicarlabel.f

parsevicarlabel.o:	parsevicarlabel.r
		$(RF) $(SP_RFLAGS) parsevicarlabel.r > parsevicarlabel.f
		$(F77) $(SP_FFLAGS) -c parsevicarlabel.f
		rm parsevicarlabel.f

parseenvilabel.o: parseenvilabel.r
		$(RF) $(SP_RFLAGS) parseenvilabel.r > parseenvilabel.f
		$(F77) $(SP_FFLAGS) -c parseenvilabel.f
		rm parseenvilabel.f

wrtenvihdr.o:	wrtenvihdr.r multmap.h tricube.h
		$(RF) $(SP_RFLAGS) wrtenvihdr.r > wrtenvihdr.f
		$(F77) $(SP_FFLAGS) -c wrtenvihdr.f
		rm wrtenvihdr.f

str2tmonth.o:	str2tmonth.r
		$(RF) $(SP_RFLAGS) str2tmonth.r > str2tmonth.f
		$(F77) $(SP_FFLAGS) -c str2tmonth.f
		rm str2tmonth.f

read2sheet.o:	read2sheet.r
		$(RF) $(SP_RFLAGS) read2sheet.r > read2sheet.f
		$(F77) $(SP_FFLAGS) -c read2sheet.f
		rm read2sheet.f

setupreadsheet.o:	setupreadsheet.r
		$(RF) $(SP_RFLAGS) setupreadsheet.r > setupreadsheet.f
		$(F77) $(SP_FFLAGS) -c setupreadsheet.f
		rm setupreadsheet.f

strdate.o:	strdate.r
		$(RF) $(SP_RFLAGS) strdate.r > strdate.f
		$(F77) $(SP_FFLAGS) -c strdate.f
		rm strdate.f

rstbgn.o:	rstbgn.r
		$(SSPP) -r -$(SSPPFLAGS) rstbgn.r > rstbgn.x
		$(RF) $(SP_RFLAGS) rstbgn.x > rstbgn.f
		$(F77) $(SP_FFLAGS)  -c rstbgn.f
		rm rstbgn.f rstbgn.x

#i4pack4.o:	i4pack4.c
#		$(SSPP) -c -$(SSPPFLAGS) i4pack4.c > tmpi4pack4.c
#		$(CC) $(SP_CFLAGS) -c -o i4pack4.o tmpi4pack4.c
#		rm tmpi4pack4.c

i4pack4.o:	i4pack4.r
		$(SSPP) -r -$(SSPPFLAGS) i4pack4.r > i4pack4.x
		$(RF) $(SP_RFLAGS) i4pack4.x > i4pack4.f
		rm i4pack4.x
		$(F77) $(SP_FFLAGS) -c i4pack4.f
		rm i4pack4.f

delhist.o:	delhist.r
		$(RF) $(SP_RFLAGS) delhist.r > delhist.f
		$(F77) $(SP_FFLAGS) -c delhist.f
		rm delhist.f

tri1setup.o:	tri1setup.r tri1.h multmap.h
		$(RF) $(SP_RFLAGS) tri1setup.r > tri1setup.f
		$(F77) $(SP_FFLAGS) -c tri1setup.f
		rm tri1setup.f

reflsetup.o:	reflsetup.r tricube.h tri1.h multmap.h
		$(RF) $(SP_RFLAGS) reflsetup.r > reflsetup.f
		$(F77) $(SP_FFLAGS) $(BSLASH) -c reflsetup.f
		rm reflsetup.f

cubecorder.o:	cubecorder.r tricube.h tri1.h multmap.h obuffers.h
		$(RF) $(SP_RFLAGS) cubecorder.r > cubecorder.f
		$(F77) $(SP_FFLAGS) -c cubecorder.f
		rm cubecorder.f

specorder.o:	specorder.r tricube.h tri1.h multmap.h
		$(RF) $(SP_RFLAGS) specorder.r > specorder.f
		$(F77) $(SP_FFLAGS) -c specorder.f
		rm specorder.f

tp1mat.o:	tp1mat.r tricube.h tri1.h multmap.h
		$(RF) $(SP_RFLAGS) tp1mat.r > tp1mat.f
		$(F77) $(SP_FFLAGS2) $(FOPT2) -c tp1mat.f
		rm tp1mat.f

tp1all.o:	tp1all.r tricube.h tri1.h multmap.h
		$(RF) $(SP_RFLAGS) tp1all.r > tp1all.f
		$(F77) $(SP_FFLAGS2) $(FOPT2) -c tp1all.f
		rm tp1all.f

flushseqfile.o:	flushseqfile.r
		$(RF) $(SP_RFLAGS) flushseqfile.r > flushseqfile.f
		$(F77) $(SP_FFLAGS) -c flushseqfile.f
		rm flushseqfile.f

opencube.o:	opencube.r
		$(RF) $(SP_RFLAGS) opencube.r > opencube.f
		$(F77) $(SP_FFLAGS) -c opencube.f
		rm opencube.f

wrtspcrdrout.o:	wrtspcrdrout.r multmap.h
		$(RF) $(SP_RFLAGS) wrtspcrdrout.r > wrtspcrdrout.f
		$(F77) $(SP_FFLAGS) -c wrtspcrdrout.f
		rm wrtspcrdrout.f

wrtcrdrout.o:	wrtcrdrout.r multmap.h
		$(RF) $(SP_RFLAGS) wrtcrdrout.r > wrtcrdrout.f
		$(F77) $(SP_FFLAGS) -c wrtcrdrout.f
		rm wrtcrdrout.f

gcsetup.o:	gcsetup.r multmap.h
		$(RF) $(SP_RFLAGS) gcsetup.r > gcsetup.f
		$(F77) $(SP_FFLAGS) -c gcsetup.f
		rm gcsetup.f

wrtgcsetup.o:	wrtgcsetup.r tricube.h tri1.h multmap.h
		$(RF) $(SP_RFLAGS) wrtgcsetup.r > wrtgcsetup.f
		$(F77) $(SP_FFLAGS) -c wrtgcsetup.f
		rm wrtgcsetup.f

creatoutfiles.o:	creatoutfiles.r tricube.h tri1.h multmap.h
		$(RF) $(SP_RFLAGS) creatoutfiles.r > creatoutfiles.f
		$(F77) $(SP_FFLAGS) -c creatoutfiles.f
		rm creatoutfiles.f

cublineout.o:	cublineout.r tricube.h tri1.h multmap.h obuffers.h
		$(RF) $(SP_RFLAGS) cublineout.r > cublineout.f
		$(F77) $(SP_FFLAGS) -c cublineout.f
		rm cublineout.f

nvres1mat.o:	nvres1mat.r tricube.h tri1.h multmap.h
		$(RF) $(SP_RFLAGS) nvres1mat.r > nvres1mat.f
		$(F77) $(SP_FFLAGS) -c nvres1mat.f
		rm nvres1mat.f

tp1cse.o:	tp1cse.r tricube.h tri1.h multmap.h
		$(RF) $(SP_RFLAGS)  tp1cse.r > tp1cse.f
		$(F77) $(SP_FFLAGS2) $(FOPT2) -c tp1cse.f
		rm tp1cse.f

