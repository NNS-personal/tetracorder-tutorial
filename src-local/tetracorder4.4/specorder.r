	subroutine specorder

	implicit integer*4 (i-n)

#ccc  name:         cubecoredr
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: analyze features in a spectrum with the tricorder algorithm
#ccc
#ccc  algorithm description: See Clark et al, 1990, JPL AVIRIS Conf.
#ccc  system requirements: Unix
#ccc  subroutines called: many specpr routines, need specpr.a library
#ccc  argument list description: see below
#ccc  parameter description: see below
#ccc  common description: see below
#ccc  message files referenced: none
#ccc  internal variables: see below
#ccc  file description: see below
#ccc  user command lines: see below
#ccc  update information: see below
#ccc  NOTES:
#ccc
#ccc---------------------------------------------------------------


	include 	"../specpr/src.specpr/common/label1"
	include 	"../specpr/src.specpr/common/lbl3"
	include 	"../specpr/src.specpr/common/lbl4"
	include 	"../specpr/src.specpr/common/lbl7"
	include 	"../specpr/src.specpr/common/lundefs"
	include 	"../specpr/src.specpr/common/alphabet"
	include 	"../specpr/src.specpr/common/cmd"
	include 	"../specpr/src.specpr/common/lblg"
	include 	"../specpr/src.specpr/common/lblwav"
	include 	"../specpr/src.specpr/common/cmdarg"
	include 	"../specpr/src.specpr/common/dscrch"
	include 	"../specpr/src.specpr/common/ioftyp"
	include 	"../specpr/src.specpr/common/blank"
	include		"../specpr/src.specpr/common/lblvol"

# arrays for multiple materials

	include "multmap.h"

	include "tricube.h"

# basic tricorder parameters

        include "tri1.h"

	character*40 otitl2

	integer*4 ikl
#RED
	integer*4 fnb     # function
	integer*4 lnb     # function

# changed 'call er'to 'call eralph'
# segmentation-fault error when display set to X-Windows
#    due to a screen clear 'XWIN call xclear'
#    in 'src.specpr/hpgraph/respag.r'
#69	call er
69	call eralph

	call whedr

	write(ttyout,70)
70      format (///,40('-'),/,
			' Enter file id and record number for',
			' the SPECTRUM to be analyzed',/,10x,
                        ' (then a  d  to delete points)',/,
			' or e or x to exit.'/)

	chdltflg=' '  # default: no delete points
	call crtin
	i = 1
	call wjfren(i,x1,idevs)
	if (i >= 79 && idevs == 0) {
		call what(i)
		call crtin
		go to 69
	}
	call wjfren(i,xfilb,ic2)

	if (ic2 == ihd) {    # want to delete points
		chdltflg='d'
		ic2=0
	}

#        *** check for hard or soft exit ***
	if (ic2==ihx || ic2==ihe)  {
		ic=ic2
		icrst = 1
		call rstart(icrst)
		return
	} else if (idevs==ihx || ic2==ihx) {
		ic=ic2
		icrst = 1
		call rstart(icrst)
		return

#       *** check for invalid input ***
	} else if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
		write(ttyout,66)
66		format ('invalid input, reenter')
		call crtin
		go to 69

#       *** looks ok so get file s ***
	} else {
		ifils = xfilb
		call devok (4,idevs,ifils,lun,ier)
		if (ier!=0) {
			write (ttyout,*) 'ERROR: invalid file+record no.'
			write (ttyout,*) '       press return to try again'
			call crtin
			go to 69
		}
		itmp = ifils
		call redfil(itmp,lun,ier)
		if (ier != 0) {
			write (ttyout,*) 'ERROR: reading file+record no.'
			write (ttyout,*) '       press return to try again'
			call crtin
			go to 69
		}
		iform = 1
		write (ttyout,68) idevs, ifils, ititl
68		format (' Spectrum:','  ',a,i7,5x,a,//)
		call namdev (idevs, inamr)
	}

	call wjfren(i,x1,ic2)  # check for delete points indicator
	if (ic2 == ihd) {    # want to delete points
		chdltflg='d'
		ic2=0
	}

# write history
	write (lunhist,3050) ihbcksl, idevs, ifils,chdltflg, ihbcksl,
				 inamr, ihbcksl, ititl
3050    format (a,'#',17x,'Analyzing spectrum:',//,
                a1,i7,1x,a,14x,a,
                '# file ID, rec no. (',a,')'/,
                a,'#',11('=-'),' TITLE=',a)

# enter thresholding

3139	write (ttyout,3140)
3140	format ('Enter the the minumum and maximum data thresholds.',//,
                5x,'The thresholds are in real number (scaled) values, and',/,
                5x,'if = 0.0  no thresholds are set')

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		return
	}
	if (il != 0) {
		call what(i)
		go to 3139
	}
        thrshmin=-1.0e35
        thrshmax= 1.0e35

        call wjfren (i,x,il)
        if (il != 0) {
                call what (i)
                go to 3139
        }
        if(abs(x) > 0.1e-20) thrshmin = x

        call wjfren (i,x,il)
        if (il != 0) {
                call what (i)
                go to 3139
        }
        if(abs(x) > 0.1e-20) thrshmax = x

#	write history

	if (thrshmin < -1.0e34 & thrshmax > 1.0e34) {
		write (lunhist, 3138) ihbcksl
3138		format (30x,a,'# no min or max data thresholds')
	} else {
		write (lunhist, 3137) thrshmin,thrshmax,ihbcksl
3137		format (f12.6,f12.6,15x,a,'# min, max data thresholds')
	}


# determine how much to output to user.

	write (ttyout,3120)
3120	format ('Enter the amount of output you desire:',/,
		'      <return> for one-line answers only',/,
		'          1    for abreviated answers only',/,
		'          2    for weighted fit + answer',/,
		'          3    for full diagnostic output:',/,
		'                   (individual fits, weighted fits',/,
		'                   and answer')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		return
	}
	if (il != 0) {
		call what(i)
		go to 3059
	}
	diaflg = nint(x)
	if (diaflg < 0) diaflg = 0
	if (diaflg > 3) diaflg = 3

	write (lunhist, 3122) diaflg,ihbcksl
3122	format (i3,15x,a,'# diagnostic level of output')

	if (diaflg > 0) {
		write (lunresult,3051) inamr, ifils,chdltflg, ititl
3051		format (40('-='),/,27x,'Analyzing spectrum:',/,
		a,i7,1x,a,15x,a)
	}

# delete points

	if (chdltflg == 'd') {
		write (ttyout,*) 'Enter channels to delete, then a c to continue'
		call crtin
		i=1
		call dltpts(i,jdlt,idlt,nchans,ic2)
		if (ic2==ihx)  {
			icrst = 1
			call rstart(icrst)
			return
		}
		if (ic2!=ihe & jdlt > 1) {
			mxstrdel=240   # 3 lines of 80 characters
			call delhist(jdlt,idlt,mhist,mxstrdel)

			write (lunhist,3055) mhist(1:lnb(mhist(1:80)))
			write (lunresult,3055) mhist(1:lnb(mhist(1:80)))
3055			format (a)
			if (mhist(81:81) != ' ') {
				write (lunhist,3055) mhist(81:lnb(mhist(81:160)))
				write (lunresult,3055) mhist(81:lnb(mhist(81:160)))
			}
			if (mhist(161:161) != ' ') {
				write (lunhist,3055) mhist(161:lnb(mhist(161:240)))
				write (lunresult,3055) mhist(161:lnb(mhist(161:240)))
			}
			do jj = 1, jdlt {               # delete the channels
				data(idlt(jj))=-1.23e34
			}
		} else {
			write (lunhist,3055) 'c'
			write (lunresult,3055) 'c'
		}

	}

# Title 

	write (ttyout,162)
162	format (' Type in an output comment',/,
		'---------------------------------------|')
	call crtin
	jfirst=fnb(iopcon)
	if (jfirst <1 1) jfirst = 1 # no blank at beginning of line
	jend=lnb(iopcon)
	if (jend == 0) jend = 80 # no blank at end of line
	length = jend - jfirst + 1
	if (length > 40) length = 40
	jend = jfirst + length - 1
	otitl2(1:length) = iopcon(jfirst:jend)
	if (length < 40) otitl2(length+1:40)=' '
	do ij = 1, 40 {
		if(otitl2(ij:ij) == char(0)) {
			otitl2(ij:ij) = ' '
		}
	}

# write history
3059	write (lunhist, 3060) otitl2,ihbcksl
3060	format (a, 2x,a,'# comment ')
	write (lunresult, 3060) otitl2,ihbcksl

################
nth = 1    # print the spectrum analyzed in single spectrum mode
xel = 1    # one pixel only in single spectrum mode
yel = 1    # one line only in single spectrum mode

# set stats to zero

	do i = 1, nmats {
		statsmapfit(i) = 0
		statsmapdepth(i) = 0
		statsmapfd(i) = 0
		fitmean(i) = 0
	}

	icubflg = 0
        # check thresholds and delete out of min max range

        if (thrshmin > -1.0e34 | thrshmax < 1.0e34 ) {  # do range check
                do ikl = 1, nchans {
                        if (data(ikl) < thrshmin) {
                                data(ikl) = -1.23e34
				#write (ttyout,*) 'DEBUG: NOTE ch:',ikl,
				#		' below threshold: DELETED'
                        }
                        if (data(ikl) > thrshmax) {
                                data(ikl) = -1.23e34
				#write (ttyout,*) 'DEBUG: NOTE ch:',ikl,
				#		' above threshold: DELETED'
                        }
                }
        }



######## now analyze the spectrum #########################

	call tp1all (icubflg,data)

###########################################################
# pause so user can see the result.

	write (ttyout,*) 'Press return to continue'
	call crtin


	if (noflush == 1) {    # flush buffer
		write (ttyout, *) 'Flushing results buffer'
		call flushseqfile (lunresult,
			resultfile(1:lnb(resultfile)), ier)
		if (ier != 0) {
			call what (-1)
			return
		}

# End program

		write (ttyout,*)'Updating restart file.'
		icrst = 1
		call rstart (icrst)
	}
	return
	end
