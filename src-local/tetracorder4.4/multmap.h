# header file multmap.h
#
# Roger N. Clark, USGS

	integer*4       maxmat
	integer*4       maxmat1
        integer*4       maxfeat
        integer*4       maxnotfeat
        integer*4       maxfeatratio
        integer*4       imaxch
        integer*4       maxpix
	integer*4	maxpi2
	integer*4	maxpi4
        integer*4       maxgrp
        integer*4       maxcse
        integer*4       maxgrpcse
	integer*4	mfilelen
	integer*4	maxaltlib
	integer*4	lund
	integer*4	lunf
	integer*4	lunfd
	integer*4	luntr

# NOTE: if the parameters below, maxpix, imaxch, others?, you must modify tricube.h 

	parameter       (maxmat=600)   # maximum materials
	parameter       (maxmat1=400)  # maximum materials in one group/case
        parameter       (maxfeat=24)    # maximum features per material
        parameter       (maxnotfeat=8) # maximum NOT features per material
        parameter       (maxfeatratio=8) # maximum feature ratios per material

#########################################################################
####### NOTE: if you change imaxch or maxpix in multmap.h you must
#######                    also edit read2sheet.r and  tricube.h
#######                    if you are reading cubes
#######                    i2sht and chibuff sizes must match
#######                    i4buf array size = maxpix
### NOTES: if you change:
###
###            maxpix  parameter                   in multmap.h
###            imaxch  parameter                   in multmap.h
###
###    then you must also change:
###
###            maxpi2  parameter                   in multmap.h
###            maxpi4  parameter                   in multmap.h
###
###
###    Changing above has ripple effects in: (will be set by above parameters)
###
###            i4buf*  arrays should = maxpix      in tricube.h
###            chbuf*  #chars should = maxpix*4    in tricube.h
###            i2sht   array  should = maxpix      in tricube.h
###            chibuff #chars should = maxpix*2    in tricube.h
###
###            chibuff #chars should = maxpix*2    in read2sheet.r
###            i2sht   array   should = maxpix     in read2sheet.r
###            specdat array  should = maxpix      in read2sheet.r

######## select one of the following:
#########################################################################

####### use the following set of lines for image cubes
####### NOTE: as of Tetracorder 4.4, these 4 parameters below 
#######       need changing when chaning cube array sizes, as well as
#######       setting characters*X  where X = maxpi4 in the following:
#######
#######       tricube.h:
#######                  character*48000         chbuff,chbuf2,chbuf3,chbuf0
#######                  character*48000         chibuff(imaxch), chdata
#######
#######     read2sheet.r:
#######                  character*48000 charline
#######

        parameter       (imaxch=710)   # maximum channels in spectrum
##NOTE: parameter       (imaxch=1444)   # maximum channels in spectrum 
##                                      # at 32765 pixels, this is the max 
##                                      # channels before hitting a 2 GByte limit

##### use this for image cubes:
        parameter       (maxpix=32765) # maximum pixels per line
        parameter       (maxpi2=65530) # = maxpix *2
        parameter       (maxpi4=131060) # = maxpix *4
# or:
######## NOTE: use the following two lines for Nicolet single spectra analysis
#        parameter       (imaxch=4852)   # maximum channels in spectrum
#        parameter       (maxpix=1) # maximum pixels per line
#        parameter       (maxpi2=2) # = maxpix *2
#        parameter       (maxpi4=4) # = maxpix *4
#########################################################################

        parameter       (maxgrp=30)   # maximum number of spectral groups
        parameter       (maxcse=30)   # maximum number of spectral cases
        parameter       (maxgrpcse=60)# maximum number of cases + groups
				      # NOTE: maxgrpcse = maxgrp+maxcse
				      # A MUST==========================

	parameter	(mfilelen=50) # max length of output file names
                                      # if change, must change values below

	parameter	(maxaltlib=9) # number of alternate library keywords

	parameter	(lund=56)     # lun for depth file
	parameter	(lunf=57)     # lun for fit file
	parameter	(lunfd=58)    # lun for fit*depth file
	parameter	(luntr=59)    # lun for tricorder-format file

        common /rlib0/ rlb
        real*4    rlb(imaxch,maxmat)         # reflectance of spectra in library


        common /rlib1/ rlbc
        real*4    rlbc(imaxch,maxfeat,maxmat)# continuum removed lib features

######## common block for all the details in the algorithm

        common /multmap/ zcontmn,zcontmx
        common /multmap/ zcontlmn,zcontlmx,zcontrmn,zcontrmx
        common /multmap/ zcontlgtr,zcontrgtl
        common /multmap/ nfeat,nmats,dl,dln,dlbar,dlsum
        common /multmap/ cchans,nchmin,nchmax,nftype
        common /multmap/ zfit,zdepth,zfd,zcompf,featimprt
        common /multmap/ tfit,tdepth,tfd
        common /multmap/ ofit,odepth,ofd,fitmean
	common /multmap/ bdscal,qfscal
	common /multmap/ statsmapfit,statsmapdepth,statsmapfd
	common /multmap/ thrshfit,thrshfitall,thrshdepth
        common /multmap/ thrshdepthall,thrshdepthfit
	common /multmap/ thrshfd,thrshfdall,thrshfdfit,thrshfddepth
	common /multmap/ numnotfeat,notfid,notrec
	common /multmap/ notmat,notfeat,notfrn,notflg
	common /multmap/ thrshdnot,thrshfnot
	common /multmap/ ngroups, group, group0, ncases, icase
	common /multmap/ incgrp0,nzgroup,nzcase
	common /multmap/ igbest,grpbest,csebest,icsbest
	common /multmap/ grpsecnd,csesecnd,icssecnd
	common /multmap/ matgrp, matcse,nmatgrp,nmatcse
        common /multmap/ ialgorithm, iaction, mclass
        common /multmap/ featidratio, nfeatratio
	common /multmap/ udata
	common /multmap/ uratio,rratio,flguratio,flgrratio
	common /multmap/ dclass, mpressure, mtemp
	common /multmap/ pathsnd, sound1fil, dosound
	common /multmap/ zrcbblcgt, zrcbblclt, zlcbbrcgt, zlcbbrclt
	common /multmap/ zrtimesbd
	common /multmap/ featratio



# Arrays for mapping multiple materials at same time

        integer*4 ialgorithm(maxmat) # algorithm number:
					# = 0: tricorder-primary
					# = 1: nvres (veg red edge)
        integer*4 nfeat(maxmat)    # number features to analyze in each material
        integer*4 nmats            # number of materials
        integer*4 ngroups          # number of groups
        integer*4 ncases           # number of cases
	integer*4 nzgroup          # highest non-zero group
	integer*4 nzcase           # highest non-zero case

	######### materials in each group and case: ########################

        integer*4 matgrp(maxmat1,0:maxgrp)  # cross reference list for groups
        integer*4 matcse(maxmat1,maxcse)  # cross reference list for cases
        integer*4 nmatgrp(0:maxgrp)        # number of materials in each group
        integer*4 nmatcse(maxcse)        # number of materials in each case

	####################################################################

	integer*4 iaction(maxmat,maxcse) # what action to take when a merarial
					 # is found: this is what case to do.

	####################################################################

	real*4    dl(maxfeat,maxmat)         # ref lib band depth areas
	real*4    dln(maxfeat,maxmat)        # ref lib normlzd band depth areas
	real*4    dlbar(maxmat)              # ref lib, mean band depth areas

	real*4    dlsum(maxmat)              # ref lib, sum of band depth areas

        integer*4 cchans(4,maxfeat,maxmat)   # continuum chans for each material
        integer*4 nchmin(maxfeat,maxmat) # minimum channel in each feature
        integer*4 nchmax(maxfeat,maxmat) # maximum channel in each feature
        integer*4 nftype(maxfeat,maxmat) # feature type (1=absorb, -1=emission)
        integer*4 group0(maxmat)         # group 0 suppression enable = 1 (no 0)
					 #    = 0 (do group 0 with other groups)
        integer*4 group(maxmat)          # group number for each material
					 #       group numbers < 0 are cases
        integer*4 icase(maxmat)          # case number for each material
					 #       case numbers <= 0 are groups
	integer*4 incgrp0(maxgrp)        # include group 0 when in analysis
	integer*4 grpbest(maxgrp)        # the best material in each group
	integer*4 igbest(maxgrp,maxpix)  # the best material in each group,
					 #     for each pixel in the line.
	integer*4 csebest(maxcse)        # the best material in each case
	integer*4 icsbest(maxgrp,maxpix) # the best material in each case,
					 #     for each pixel in the line.

	integer*4 grpsecnd(maxgrp)        # the second best material 
                                          # in each group
#	integer*4 igsecnd(maxgrp,maxpix)  # the second best material 
# we don't need this unles we do output   # in each group,
#					  #     for each pixel in the line.
	integer*4 csesecnd(maxcse)        # the second best material 
                                          # in each case
	integer*4 icssecnd(maxgrp,maxpix) # the second best material 
                                          # in each case,
					  #     for each pixel in the line.

        real*4    zcontmn(maxfeat,maxmat)# contin min for each feature, material
                                         #        continuaa <min: output deleted
        real*4    zcontmx(maxfeat,maxmat)# contin max for each feature, material
                                         #        continuaa >max: output deleted
        real*4    zcontlmn(maxfeat,maxmat)# right continuum min for each 
                                         #        feature, material
                                         #        continuaa <min: output deleted
        real*4    zcontlmx(maxfeat,maxmat)# right continuum max for each
                                         #        feature, material
                                         #        continuaa >max: output deleted
        real*4    zcontrmn(maxfeat,maxmat)# right continuum min for each 
                                         #        feature, material
                                         #        continuaa <min: output deleted
        real*4    zcontrmx(maxfeat,maxmat)# right continuum max for each
                                         #        feature, material
                                         #        continuaa >max: output deleted
        real*4    zcontlgtr(2,maxfeat,maxmat)# left continuum must be > 
                                           # right continuum by this value in
                                           # order to be valid match
                                           # 2 values = min max fuzzy logic
        real*4    zcontrgtl(2,maxfeat,maxmat)# right continuum must be > 
                                           # left  continuum by this value in
                                           # order to be valid match
                                           # 2 values = min max fuzzy logic

#       the following group restricts the right-continuum to band-bottom to
#                 left-continuum angle before continuum removal.
#                 It allows identification restriction of pre-continuum
#                 removed shapes.  2 values are used for fuzzy logic.

        real*4    zrcbblcgt(2,maxfeat,maxmat)# (rc-bb)/(lc-bb) > fuzz(1,2)
                                           # rc =right continuum
                                           # bb = band bottom
                                           # lc = left continuum
                                           # must be > limits 1, 2
                                           # in order to be valid match
                                           # 2 values = min max fuzzy logic
        real*4    zrcbblclt(2,maxfeat,maxmat)# (rc-bb)/(lc-bb) < fuzz(1,2)
                                           # must be < limits 1, 2
                                           # in order to be valid match
                                           # 2 values = min max fuzzy logic
        real*4    zlcbbrcgt(2,maxfeat,maxmat)# (lc-bb)/(rc-bb) > fuzz(1,2)
                                           # must be > limits 1, 2
                                           # in order to be valid match
                                           # 2 values = min max fuzzy logic
        real*4    zlcbbrclt(2,maxfeat,maxmat)# (lc-bb)/(rc-bb) < fuzz(1,2)
                                           # must be < limits 1, 2
                                           # in order to be valid match
                                           # 2 values = min max fuzzy logic

        real*4    zrtimesbd(2,maxfeat,maxmat)# reflectance*band depth < fuzz(1,2)
                                           # must be < limits 1, 2
                                           # in order to be valid match
                                           # 2 values = min max fuzzy logic

        real*4    zfit(maxfeat,maxmat)   # fits for each feature, material
        real*4    zdepth(maxfeat,maxmat) # depths for each feature, material
        real*4    zfd(maxfeat,maxmat)    # fit*depth for each feature, material
        real*4    zcompf(maxfeat,maxmat) # feature compatible factor:
                                         #         = 1 expected abs, got abs
                                         #         = 1 expected emis, got emis
                                         #         =-1 expected abs, got emis
                                         #         =-1 expected emis, got abs

        integer*4 featimprt(maxfeat,maxmat)  # importance of each feat, material
					     # class is a letter on command line
					     #  = 0 standard feature,   class O
					     #      optionally present
					     #      counts in fit computation
					     #  = 1 weak feature,       class W
					     #      must be present
					     #      does not count in fit
					     #  = 2 diagnostic feature, class D
					     #      must be present, if not
					     #      this material cant be
					     #      present

	real*4 thrshfit(2,maxmat)       # threshold to fits
					# command line keyword: FIT>
                                        # 2 values = min max fuzzy logic

	real*4 thrshfitall(2,maxmat)    # threshold to fits and apply to all
					# command line keyword: FITALL>
                                        # 2 values = min max fuzzy logic

	real*4 thrshdepth(2,maxmat)     # threshold to depths
					# command line keyword: DEPTHALL>
                                        # 2 values = min max fuzzy logic

	real*4 thrshdepthall(2,maxmat)  # threshold to depths and apply to all
					# command line keyword: DEPTH>
                                        # 2 values = min max fuzzy logic

	real*4 thrshdepthfit(2,maxmat)  # threshold on fit and apply to depths
					# command line keyword: DEPTH-FIT>
                                        # 2 values = min max fuzzy logic

	real*4 thrshfd(2,maxmat)        # threshold to fit*depths
					# command line keyword: FD>
                                        # 2 values = min max fuzzy logic

	real*4 thrshfdall(2,maxmat)     # threshold to fit*depths and apply to all
					# command line keyword: FDALL>
                                        # 2 values = min max fuzzy logic

	real*4 thrshfdfit(2,maxmat)     # threshold to f*d fits
					# threshold is on fit and
					# applied to f*d
					# command line keyword: FD-FIT>
                                        # 2 values = min max fuzzy logic

	real*4 thrshfddepth(2,maxmat)   # threshold to f*d depths
					# threshold is on depth and
					# applied to f*d
					# command line keyword: FD-DEPTH>
                                        # 2 values = min max fuzzy logic

        real*4    tfit(0:maxgrpcse)   # fits for one pixel, one group or case
        real*4    tdepth(0:maxgrpcse) # depths for one pixel, one group or case
        real*4    tfd(0:maxgrpcse)    # fit*depth for 1 pixel, one group or case

        real*4    ofit(maxmat,maxpix)    # output fits for line pixel, material
        real*4    odepth(maxmat,maxpix)  # output depths for line pixl, material
        real*4    ofd(maxmat,maxpix)     # output fit*depth for ln pxl, material
	real*8    fitmean(maxmat)	 # mean fit values for each material

	real*4    bdscal(maxmat)         # band depth scale factor per material
	real*4    qfscal(maxmat)         # fit scale factor per material

		# Note:  there are statsmap for each fit, depth, and f*d
		# because thresholding could make each one have a
		# different number of pixels.

	integer*4 statsmapfit(maxmat)    # statistics on how many pixels
                                         # of each fit material mapped.
	integer*4 statsmapdepth(maxmat)  # statistics on how many pixels
                                         # of each depth material mapped.
	integer*4 statsmapfd(maxmat)     # statistics on how many pixels
                                         # of each fd material mapped.

					# NOT feature cmds work like:
					#
					# NOT v32 feat# thrshdnot thrshfnot
					#
        integer*4 numnotfeat(maxmat)    # number NOT features to analyze
					#                  in each material
	integer*4 notfid (maxnotfeat,maxmat) # file IDs for NOT features
	integer*4 notrec (maxnotfeat,maxmat) # rec numbers for NOT features
	integer*4 notmat (maxnotfeat,maxmat) # material number for NOT features
	integer*4 notfeat(maxnotfeat,maxmat) # feature number for NOT features
	integer*4 notfrn (maxnotfeat,maxmat) # NOT feature relative / absolute indicator
                                             #     if relative = number for this material
                                             #   = 0 NOT feature is absulute depth
                                             #   > 0 material feature number od
                                             #       current material: 
                                             #       if NOT feat depth / material
                                             #             feat depth > thrshdnot 
                                             #       then not this material

	integer*4 notflg (maxnotfeat,maxmat) # NOT feature flag:
						# = 0 NOT feature is not found
						# = 1 NOT feature is found
	real*4  thrshdnot(maxnotfeat,maxmat) # depth threshold for NOT features
						# if depth > thrshdnot,
						#             not this material
	real*4  thrshfnot(maxnotfeat,maxmat) # fit   threshold for NOT features
						# if fit   > thrshfnot,
						#             not this material

	integer*4 featidratio (2,maxfeatratio,maxmat) # features in the ratio
							# 1 = numerator
							# 2 = denominator
							# e.g. f5/f8

	integer*4 nfeatratio (maxmat) # number of feature ratios per material

	real*4 featratio(4,maxfeatratio,maxmat)	# feature strength ratio:
						#  4 values:
					# min, max with fuzzy logic
					# e.g. fratio: 1 / 8 = .2 .3 .7 .8

	integer*4 mclass(maxmat)         # the class of the material.
                                         # = 1 most important materials
                                         # = 2 mixtures
                                         # = 3 are less important mixtures

	real*4 dclass(maxmat)         # the difference in class of the material.
                                      # from the second best.  if the
                                      # difference is less than this
                                      # then choose second best
                                      # unless 2nd is the same class

	real*4 mpressure(4,maxmat)    # pressure stability range, 4 values
                                      # minimum metastable, minimum stable,
                                      # maximum stable, maximum metastable
                                      # values in bars

	real*4 mtemp(4,maxmat)        # temperature stability range, 4 values
                                      # minimum metastable, minimum stable,
                                      # maximum stable, maximum metastable
                                      # values in celcius
                                      # e.g. liquid h2o = -30 0 100 150
                                      # e.g. solid  h20 = -459 -459 0 30
                                      # could split solid h2o into temp bins!

        real*4    uratio(imaxch,maxmat)  # reflectance of spectrum to ratio 
					 # into unknown spectrum
        real*4    rratio(imaxch,maxmat)  # reflectance of spectrum to ratio 
					 # into unknown spectrum
        integer*4 flguratio(maxmat)      # flag to indicate do uratio: no =0
        integer*4 flgrratio(maxmat)      # flag to indicate do rratio: no =0

        integer*4 udata(maxmat)         # what data to analyze:
					# = 0 raw (no change to input
					# = 1 radiance
					# = 2 reflectance
					# = 3 emittance


        common /iostuff/ lenfile,lengdir,lengcdir
        common /iostuff/ mtitle,otitle,outbuff
	common /iostuff/ mfile,dfile,ffile,fdfile
	common /iostuff/ pathgrp,pathcase,obits
        common /iostuff/ oenblfit,oenbldepth,oenblfd
	common /iostuff/ altlib,altuse,numaltlib
        common /iostuff/ ndevr,nrrec,ocompress
	common /iostuff/ ndevurat,nrecurat,ndevrrat,nrecrrat

        integer*4 lenfile(maxmat)        # length of each base file name
        integer*4 lengdir(maxgrp)        # length of each group directory name
        integer*4 lengcdir(maxcse)       # length of each group directory name

        character*40 mtitle(maxmat)      # title for each material
        character*40 otitle(maxmat)      # output title for each material

	integer*2 outbuff(maxpix)        # I*2 output buffer

        character mfile(maxmat)*50        # base file name (*maxfilelen)
	character dfile*116               # file names  (*maxfilelen+pathgrp+6)
	character ffile*116               # file names  (*maxfilelen+pathgrp+6)
	character fdfile*116              # file names  (*maxfilelen+pathgrp+6)

	character*60 pathgrp(0:maxgrp)      # group path name
	character*60 pathcase(maxcse)     # case path name

	integer*4 obits(maxmat)          # output bits/pixel= 8 or 16

	integer*4 ocompress(maxmat)     # compress output: no = 0, yes = 1

	integer*4 oenblfit(maxmat)	# enable output of fit image
	integer*4 oenbldepth(maxmat)	# enable output of depth image
	integer*4 oenblfd(maxmat)	# enable output of fd image

        integer*4 numaltlib             # number of alternate libraries
	character*20 altlib(maxaltlib)  # alternate library keywords
	character*20 altuse             # alternate library used

	character*1 ochbuff(maxpix)     # byte output buffer
	equivalence (outbuff,ochbuff)

        integer*4 ndevr(maxmat)         # specpr device letter ids, lib
        integer*4 nrrec(maxmat)         # specpr rec nos for lib spec

        integer*4 ndevurat(maxmat)      # specpr device letter ids for uratio
        integer*4 nrecurat(maxmat)      # specpr rec nos for uratio
        integer*4 ndevrrat(maxmat)      # specpr device letter ids for rratio
        integer*4 nrecrrat(maxmat)      # specpr rec nos for rratio

	character*60 pathsnd		# path for sound files
	character*60 sound1fil(maxmat)	# sound file name
	integer*4    dosound(maxmat)	# do sound = 1, not =0
