	subroutine reflsetup

	implicit integer*4 (i-n)

#ccc  name:         reflsetup
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: sets up the reference library features
#ccc                     for tetracorder to map with.
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
#ccc            This routine generates a band depth map

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

# basic tetracorder parameters

	include "tri1.h"

#RED
	integer*4 ihchar     # function
	logical*4 fexist     # file exists: true, false if doesn't

	integer*4 cmdverbose   # function cmdverbose
	integer*4 fnb          # function fnb
	integer*4 lnb          # function lnb
	integer*4 ictlimflg, ictrlimflg, ictllimflg
	integer*4 ichtmp2, ichtmp1, ndl
	integer*4 itmpfr, nfr

	character*1 imch(5)
	character*20 chtest
	character*110 chtmp1, chtmp2
	character cht1*3, cht2*5, cht3*2

	real*4 x

# define feature importance characters

	imch(1) = 'O'
	imch(2) = 'W'
	imch(3) = 'D'
	imch(4) = '?'
	imch(5) = '?'

	itmp =cmdverbose(-1)
######	write (ttyout,*) 'DEBUG: cmdverbose(-1) = ',itmp

###########################################################
120	if (cmdverbose(-1) <= 1) write (ttyout,125)
125	format (///,' Reference library specpr output:',/,
		'Enter:',/,
		10x,'liboutput none   or',/,
		10x,'liboutput new-file-name (NOT ENABLED YET)')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 120    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put liboutput stuff here in FUTURE
	write (lunhist,126) 'liboutput none'
126	format (a)

###########################################################
130	if (cmdverbose(-1) <= 1) write (ttyout,135)
135	format (///,' Reference library specpr features output:',/,
		'Enter:',/,
		10x,'featoutput none   or',/,
		10x,'featoutput new-file-name (NOT ENABLED YET)')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 130    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put featoutput stuff here in FUTURE
	write (lunhist,136) 'featoutput none'
136	format (a)

###########################################################
#     do in shell script driver
#1301	if (cmdverbose(-1) <= 1) write (ttyout,1351)
#1351	format (///,' Sound files directory path',/,
#		'Enter:',/,
#		10x,'soundpath "dir-path"')
#	call crtin
#	i = index(iopcon,'soundpath')
#	if ( i == 0) {
#		write (ttyout,*) 'ERROR: no soundpath keyword found'
#		go to 1301
#	}
#	i = i+9
#	call wjfren (i,x,il)
#	i = i - 1
#	isfirst = i
#	if (i > 78) {
#		write (ttyout,*)'ERROR: no sound file name'
#		go to 1301
#	}
#	do jj = i, 80 {
#		if (iopcon(jj:jj) == ' ') break # find next blank
#	}
#	islast = j - 1
#	pathsnd = iopcon(isfirst:islast)
#	#
#	write (lunhist,1361) islast
#1361	format ('soundpath ', a)
#
###########################################################
140	if (cmdverbose(-1) <= 1) write (ttyout,145) maxaltlib
145	format (' Enter the number of alternate ',
			'library reference data sets',/,
		10x,'range: 1 to',i4)
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 140    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il != 0) {
		call what(i)
		go to 210
	}
	if (x < 1 || x > maxaltlib) {
		write (ttyout,148) maxaltlib
148		format ('ERROR: Number of alternates must be',
			' > 1 and <= ',i4,/,
                        18x,'(Note: this limit can be inceased by a ',
				'simple recompile',/,
			25x,'of the program.  See your software manager')
		go to 140
	}
	numaltlib = x + 0.5
	write (lunhist,149) numaltlib, ihbcksl
149	format (i5, 20x,a1,'# munber of alternate libraries')

	do itmp = 1, numaltlib {
150		if (cmdverbose(-1) <= 1) write (ttyout,155) itmp
155		format (' Enter the alternate ',
			'library reference name',i4)
		call crtin
		i = 1
		call wjfren (i,x,il)
		ifirst=i-1
		call wjfren (i,x,il2)
		if ((il==ihx | il==ihe) & il2 == 0)  {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		ialen = len(altlib(itmp))
		ilast = lnb(altlib(itmp))
		if (ilast - ifirst + 1 > ialen) {
			call what(ifirst+ialen)
			write (ttyout,*) 'ERROR: length too long, reenter'
			go to 150
		}
		altlib(itmp) = ' '
		altlib(itmp) = iopcon(ifirst:ilast)

#############FUTURE FIX: add : after name (watch blanks

		write (lunhist,156) altlib(itmp),ihbcksl
156		format (a, 10x,a1,'# alternate library name')

	}

###########################################################
# RED - 07/07/2008 - Removed extraneous comma at end of 158 statement
157	if (cmdverbose(-1) <= 1) write (ttyout,158)
158	format (' Enter:  use alternate-name ',/,
			20x,'where alternate-name is one of ',
				'the above',/,
			20x,'alternate library reference data sets')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 157    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	ifirst = i - 1
	if (iopcon(ifirst:ifirst+2) != 'use') {
		call what(i)
		write (ttyout,*) 'ERROR: "use" keyword not found'
		go to 157
	}
	i = i+3
	call wjfren (i,x,il)
	i = i-1
	ilast = lnb(iopcon(i:80))
	if (ilast == 0) {
		call what(i)
		write (ttyout,*) 'ERROR: no alternate keyword found'
		go to 157
	}
	altuse = ' '
	altuse = iopcon(i:i+ilast)
	write (lunhist,159) altuse, ihbcksl
159	format ('use ',a,10x,a1,'# alternate library to use')

###########################################################
160	if (cmdverbose(-1) <= 1) write (ttyout,165) maxgrp, maxcse
165	format (' Enter the number of groups and ',
			'cases:',
		10x,'range: 1 to',i7,' groups (does not include group zero)',/,
		10x,'range: 0 to',i7,' cases  (first case  is one)')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 160    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il != 0) {
		call what(i)
		go to 160
	}
	if (x < 1 || x > maxgrp) {
		write (ttyout,167) maxgrp
167		format ('ERROR: Number of groups must be',
			' > 1 and <= ',i4,/,
                        18x,'(Note: this limit can be inceased by a ',
				'simple recompile',/,
			25x,'of the program.  See your software manager')
		go to 160
	}
	ngroups = x + 0.5
	call wjfren (i,x,il)
	if (il != 0) {
		call what(i)
		go to 160
	}
	if (x < 0 || x > maxcse) {
		write (ttyout,168) maxcse
168		format ('ERROR: Number of cases must be',
			' 0 to <= ',i4,/,
                        18x,'(Note: this limit can be inceased by a ',
				'simple recompile',/,
			25x,'of the program.  See your software manager')
		go to 160
	}
	ncases = x + 0.5
	write (lunhist,169) ngroups, ncases, ihbcksl
169	format (i5,3x,i5, 10x,a1,'# munber of groups and cases')

	do itmp = 1, ngroups {
170		if (cmdverbose(-1) <= 1) write (ttyout,175) itmp
175		format (' Enter the group ', i5,
			' directory path name',i4)
		call crtin
		i = 1
		call wjfren (i,x,il)
		if (i >= 80 & il == 0) go to 170    # blank line, read again
		ifirst=i-1
		call wjfren (i,x,il2)
		if ((il==ihx | il==ihe) & il2 == 0)  {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		ialen = len(pathgrp(itmp))
		ilast = lnb(iopcon)
		if (ilast - ifirst + 1 > ialen) {
			call what(ifirst+ialen)
			write (ttyout,*) 'ERROR: length too long, reenter'
			go to 170
		}
		pathgrp(itmp) = ' '
		pathgrp(itmp) = iopcon(ifirst:ilast)
		lengdir(itmp) = ilast - ifirst + 1

		write (lunhist,176) pathgrp(itmp)(1:lengdir(itmp)),
					ihbcksl,itmp
176		format (a, 10x,a1,'# group',i5,' path name')

	}

	if (ncases > 0) {
		do itmp = 1, ncases {
1801			if (cmdverbose(-1) <= 1) write (ttyout,1851) itmp
1851			format (' Enter the case ', i5,
				' directory path name',i4)
			call crtin
			i = 1
			call wjfren (i,x,il)
			if (i >= 80 & il == 0) go to 1801  # blank line, read again
			ifirst=i-1
			call wjfren (i,x,il2)
			if ((il==ihx | il==ihe) & il2 == 0)  {
				ic=il
				icrst = 1
				call rstart(icrst)
				call what (-1)
				return
			}
			ialen = len(pathcase(itmp))
			ilast = lnb(iopcon)
			if (ilast - ifirst + 1 > ialen) {
				call what(ifirst+ialen)
				write (ttyout,*) 'ERROR: length too long, reenter'
				go to 1801
			}
			pathcase(itmp) = ' '
			pathcase(itmp) = iopcon(ifirst:ilast)
			lengcdir(itmp) = ilast - ifirst + 1

			write (lunhist,1861) pathcase(itmp)(1:lengcdir(itmp)),
						ihbcksl,itmp
1861			format (a, 10x,a1,'# case',i5,' path name')

		}
	}

#####################################################################
2190	if (cmdverbose(-1) <= 1) write (ttyout,2191)
2191	format (' Enter nogroup0: and which groups should not include',
		'group 0',/,'Example: nogroup0: 3 6 8')
	do itmp = 1, maxgrp {
		incgrp0(itmp) = 1      ##### default: include group 0
	}
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 2190    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	i = i-1
	if (iopcon(i:i+8) != 'nogroup0:') {
		call what(i)
		go to 2190
	}
	i = i + 9
2195	call wjfren (i,x,il)
	if (i > 79) go to 2197  # done scanning line
	if (il != 0) {
		write (ttyout,*) 'ERROR: invalid character'
		call what(i)
		go to 2190
	}
	itmp = x + 0.5
	if (itmp < 1 | itmp > maxgrp) {
		write (ttyout,*) 'ERROR: group out of range: ', itmp
		call what(i)
		go to 2190
	} else {
		incgrp0(itmp) = 0
	}
	go to 2195

2197	write (lunhist,2192) iopcon(1:lnb(iopcon))
2192	format (a)
	
	imat = 0


#####################################################################
190	if (cmdverbose(-1) <= 1) write (ttyout,191)
191	format (' Enter BEGIN SETUP  to start setup')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 190    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (iopcon(1:11) != 'BEGIN SETUP') {
		call what(-1)
		go to 190
	}
	write (lunhist,192) 'BEGIN SETUP'
192	format (a)
	
	imat = 0


#####################################################################
############# SETUP: group, case, or END SETUP ######################

210	if (cmdverbose(-1) <= 1) write (ttyout,215)
215	format (///,' Enter:',/,
		10x,'group #  (where # is the group number),',/,
		10x,'case #   (where # is the case  number),',/,
		10x,'END SETUP  to end tetracorder setup')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 210    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il == ihce) {   # possibly END SETUP
		if (iopcon(i:i+7) == 'ND SETUP') {
			nmats = imat
			write (lunhist,216) 'END SETUP'
216	format (a)

			return
		}
		call what (i)
		go to 210
	}

#       if not end setup, then it should be a case or group

	if (il == ihg) {    # possibly group
		if (iopcon(i:i+3) == 'roup') {
			i = i +4
			call wjfren (i,x,il)
			if (il != 0) {
				call what(i)
				go to 210
			}
			ii = x + 0.5
			if (ii < 0 | ii > ngroups) {
				call what (i)
				write (ttyout,*) 'ERROR: group out of range'
				go to 210
			}
			imat = imat + 1
			if (imat > maxmat) go to 217  # imat too big
			group(imat) = ii
			group0(imat) = 0  # FUTURE: check for -0
			icase(imat)  = 0   # not a case

			write (lunhist,2171) 'group', group(imat)
2171			format (a,i3)

			go to 221   # do setup for this material
			
		} else {
			call what(i)
			go to 210
		}
	} else if (il == ihc) {    # possibly case
		if (iopcon(i:i+2) == 'ase') {
			i = i +4
			call wjfren (i,x,il)
			if (il != 0) {
				call what(i)
				go to 210
			}
			ii = x + 0.5
			if (ii < 0 | ii > ncases) {
				call what (i)
				write (ttyout,*) 'ERROR: case out of range'
				go to 210
			}
			imat = imat + 1
			if (imat > maxmat) go to 217  # imat too big
			icase(imat) = ii
			group0(imat) = -1  # no group 0 in a case
			group(imat)  = -1  # not a group

			write (lunhist,2171) 'case', icase(imat)

			go to 221   # do setup for this material
			
		} else {
			call what(i)
			go to 210
		}
	}

217	write (ttyout,218) maxmat
218	format ('ERROR: Number of material must be',
			' > 1 and <= ',i4,/,
                        18x,'(Note: this limit can be inceased by a ',
				'simple recompile',/,
			25x,'of the program.  See your software manager')
	go to 210


###########################################################
221	if (cmdverbose(-1) <= 1) write (ttyout,222)
222	format (///,' Enter data converstion: ',/,
		10x,'udata: raw         (no effect yet)',/,
		10x,'udata: radiance    (no effect yet)',/,
		10x,'udata: reflectance (no effect yet)',/,
		10x,'udata: emittance   (no effect yet)')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 221    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put udata stuff here in FUTURE
	if (iopcon(1:10) == 'udata: raw') {
		udata(imat) = 0
		write (lunhist,223) 'udata: raw'
223	format (a)

	} else if (iopcon(1:15) == 'udata: radiance') {
		udata(imat) = 1
		write (lunhist,223)  'udata: radiance'

	} else if (iopcon(1:18) == 'udata: reflectance') {
		udata(imat) = 2
		write (lunhist,223)  'udata: reflectance'

	} else if (iopcon(1:16) == 'udata: emittance') {
		udata(imat) = 3
		write (lunhist,223)  'udata: emittance'
	} else {
		call what(i)
		go to 221
	}

###########################################################
231	if (cmdverbose(-1) <= 1) write (ttyout,232)
232	format (///,' Enter convolution options: ',/,
		4x,'convolve: no         (no effect yet)',/,
		4x,'convolve: alt-keyword gaussian wav= FILE-ID REC# res= FILE-ID REC#  (not allowed yet)')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 231    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put convolve stuff here in FUTURE
	if (iopcon(1:12) == 'convolve: no') {
		write (lunhist,233) 'convolve: no'
233	format (a)

	} else {
		call what(i)
		go to 231
	}

###########################################################
241	if (cmdverbose(-1) <= 1) write (ttyout,242)
242	format (///,' Enter preratio options: ',/,
		4x,'preratio: none         (no effect yet)',/,
		4x,'preratio: URATIO FILE-ID REC#',
		4x,'preratio: RRATIO FILE-ID REC#',
		4x,'preratio: URATIO FILE-ID REC# RRATIO FILE-ID REC#  (not allowed yet)')

        # uratio(imaxch,maxmat)  # reflectance of spectrum to ratio 
	#				 # into unknown spectrum
        # rratio(imaxch,maxmat)  # reflectance of spectrum to ratio 
	#				 # into unknown spectrum
        # ndevurat(maxmat)       # specpr device letter ids for uratio
        # nrecurat(maxmat)       # specpr rec nos for uratio
        # ndevrrat(maxmat)       # specpr device letter ids for rratio
        # nrecrrat(maxmat)       # specpr rec nos for rratio
        # flguratio(maxmat)      # flag to indicate do uratio: no =0
        # flgrratio(maxmat)      # flag to indicate do rratio: no =0

	flguratio(imat) = 0
	flgrratio(imat) = 0

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 241    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}

	if (iopcon(1:9) == 'preratio:') {
		i = 10
		call wjfren (i,x,il)
		i = i - 1
		if (iopcon(i:i+3) == 'none') {
			write (lunhist,243) 'preratio: none'
243			format (a)
		} else if (iopcon(i:i+5) == 'URATIO') {

			i = i + 6
			call wjfren(i,x1,idevb)
			if (i >= 79 && idevb == 0) {
				call what(i)
				go to 241
			}

			call wjfren(i,xfilb,ic2)

			if (ic2 == ihd) {    # want to delete points
						# FUTURE
				chdltflg='d'
				ic2=0
			}

		#       *** check for invalid input ***
			if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
				call what(i)
				write(ttyout,66)
				go to 241

		#       *** looks ok so get file b ***
			} else {
				ifilb = xfilb
				call devok (4,idevb,ifilb,lun,ier)
				if (ier!=0) {
					go to 241
				}
				itmp = ifilb
				call redfil(itmp,lun,ier)
				if (ier != 0) {
					go to 241
				}
				iform = 1
				if (cmdverbose(-1) <= 2) {
					write (ttyout,244) imat, idevb,
							ifilb, ititl
				}
244				format ('URATIO spectrum for mat:',i4,
							2x,a,i7,3x,a)
				if (cmdverbose(-1) <= 1) write (ttyout,*) ' '
				call namdev (idevb, inamr)

				ndevurat(imat) = idevb
				nrecurat(imat) = ifilb
			}
			do i = 1, nchans {
				uratio(i,imat) = data(i)
			}
			flguratio(imat) = 1
			write (lunhist,245) 'preratio: URATIO', inamr,
							ifilb, ihbcksl,ititl
245			format (a,1x,a,i7,9x,a,'# ',a)
			

		} else if (iopcon(i:i+5) == 'RRATIO') {    # FUTURE

			i = i + 6
			call wjfren(i,x1,idevb)
			if (i >= 79 && idevb == 0) {
				call what(i)
				go to 241
			}

			call wjfren(i,xfilb,ic2)

			if (ic2 == ihd) {    # want to delete points
						# FUTURE
				chdltflg='d'
				ic2=0
			}

		#       *** check for invalid input ***
			if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
				call what(i)
				write(ttyout,66)
				go to 241

		#       *** looks ok so get file b ***
			} else {
				ifilb = xfilb
				call devok (4,idevb,ifilb,lun,ier)
				if (ier!=0) {
					go to 241
				}
				itmp = ifilb
				call redfil(itmp,lun,ier)
				if (ier != 0) {
					go to 241
				}
				iform = 1
				if (cmdverbose(-1) <= 2) {   
					write (ttyout,1244) imat, idevb,
							ifilb, ititl
				}
1244				format ('RRATIO spectrum for mat:',i4,
							2x,a,i7,3x,a)
				if (cmdverbose(-1) <= 1) write (ttyout,*) ' '
				call namdev (idevb, inamr)

				ndevrrat(imat) = idevb
				nrecrrat(imat) = ifilb
			}
			do i = 1, nchans {
				rratio(i,imat) = data(i)
			}
			flgrratio(imat) = 1
			write (lunhist,245) 'preratio: RRATIO', inamr,
							ifilb, ihbcksl,ititl

		} else {

			write (ttyout,*) 'ERROR: expecting URATIO or RRATIO keyword'
			call what(i)
			go to 241
		}

	} else {
		write (ttyout,*) 'ERROR: expecting preratio: keyword'
		call what(i)
		go to 241
	}

###########################################################
261	if (cmdverbose(-1) <= 1) write (ttyout,262)
262	format (///,' Enter preprocess options: ',/,
		4x,'preprocess: none         (no effect yet)',/,
		4x,'preprocess: algorithm case # (not allowed yet)')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 261    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put preprocess stuff here in FUTURE
	if (iopcon(1:16) == 'preprocess: none') {
		write (lunhist,263) 'preprocess: none'
263		format (a)

	} else {
		call what(i)
		go to 231
	}

###########################################################
271	if (cmdverbose(-1) <= 1) write (ttyout,272)
272	format (///,' Enter algorithm options: ',/,
		4x,'algorithm: tricorder-primary',/,
		4x,'algorithm: nvres             (veg red edge position)',/,
		4x,'algorithm: other algorithm          (not allowed yet)')

	ialgorithm(imat) = 0 # default = tricorder-primary
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 271    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	# put algorithm stuff here in FUTURE
	i = i -1
	if (iopcon(i:i+9) == 'algorithm:') {
		i = i +10
		call wjfren (i,x,il)
		i = i -1
		if (i < 80 - 17) {
			if (iopcon(i:i+16) == 'tricorder-primary') {
				ialgorithm(imat) = 0
				write (lunhist,273) 'tricorder-primary'
273				format ('algorithm: ',a)
			} else if (iopcon(i:i+16) == 'nvres') {
				ialgorithm(imat) = 1
				write (lunhist,273) 'nvres'
			} else {
				call what(i)
				go to 231
			}
		} else {

			call what(i)
			go to 231
		}

	} else {
		call what(i)
		go to 231
	}

###########################################################
#     ********************************************
#     * get library data file
#     ********************************************

	inumalt = 0    # number of alternate library lines processed
	iflgalt = 0    # alternate library entry found
	chdltflg=' '   # default: no delete points

69      if (cmdverbose(-1) <= 1) write(ttyout,70) imat
70      format (///,40('-'),/,
			' Enter:',/,
			' Alternate Library Name: ',
			'file id and record number for',
			' REFERENCE SPECTRUM',i4,/,
                        5x,'(then an optional  d  to delete points)',/,
			5x,'Example:   AVIRIS: v 23 d',/,
			' or e or x to exit.'/)

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 69    # blank line, read again
	if ((il==ihx || il==ihe) & (iopcon(i:i) == ' ')) {
		ic=il
		icrst = 1
		call rstart(icrst)
		return
	}
	i = 1
	call wjfren (i,x,il)
	if (i >= 79 && il == 0) {
		call what(i)
		go to 69
	}
	# get alternate library

	i = i -1
	ilast = index (iopcon,':')
	if (ilast == 0) {
		call what(i)
		write (ttyout,*) 'ERROR: no ":" after alternate keyword'
		go to 69
	}
	chtest = ' '
	iflgalt = 0
	chtest = iopcon(i:ilast-1)
	i = ilast +1
	do iitmp = 1, numaltlib {  # check if keyword is valid alt lib

		if (chtest == altlib(iitmp)) {

			iflgalt = 1
		}
			
	}
	if (iflgalt == 0) { # invalid keyword
		call what(-1)
		write (ttyout,*) 'ERROR: ',chtest,' is not recognised',
				' as an alternate library'
		go to 69
	}
	if (chtest != altuse) {          # not this entry, so go to next
		write (lunhist,*) iopcon
		inumalt = inumalt + 1
		if (inumalt < numaltlib) {

			if (cmdverbose(-1) <= 1) {
				itmp1=lnb(chtest)
				itmp2=lnb(altuse)
				write(ttyout,67) chtest(1:itmp1),
						altuse(1:itmp2)
67				format ('alternate library ',a,
					'is not the in use alternate library',
					a,' select another')
			}
			go to 69
		}
		itmp1=lnb(chtest)
		itmp2=lnb(altuse)
		if (cmdverbose(-1) <= 1) {
			write(ttyout,671) chtest(1:itmp1),
				altuse(1:itmp2)
671			format ('alternate library ',a,
			'is not the "in use alternate library" ',
					a,/,' going to next command line')
		}
		if (chdltflg == ' ' ) {
			go to 161    # next command line
		} else {
			go to 3051   # delet points
		}
	} else {
		if (cmdverbose(-1) <= 1) {
			itmp1=lnb(chtest)
			itmp2=lnb(altuse)
			write (ttyout,*) 'Using ',altuse(1:itmp2)
		}
	}

	inumalt = inumalt + 1

	call wjfren(i,x1,idevb)
	if (i >= 79 && idevb == 0) {
		call what(i)
		go to 69
	}

	call wjfren(i,xfilb,ic2)

	if (ic2 == ihd) {    # want to delete points
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: deleted point flag set'
		chdltflg='d'
		ic2=0
	}

#        *** check for hard or soft exit ***
	if (ic2==ihx || ic2==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	} else if (idevb==ihx || ic2==ihx) {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return

#       *** check for invalid input ***
	} else if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
		call what(i)
		write(ttyout,66)
66		format ('invalid input, reenter')
		go to 69

#       *** looks ok so get file b ***
	} else {
		ifilb = xfilb
		call devok (4,idevb,ifilb,lun,ier)
		if (ier!=0) {
			write(ttyout,66)
			call what(i)
			go to 69
		}
		itmp = ifilb
		call redfil(itmp,lun,ier)
		if (ier != 0) {
			go to 69
		}
		iform = 1
		if (cmdverbose(-1) <= 2) {
			write (ttyout,68) imat, idevb, ifilb, ititl
		}
68		format ('Reference spectrum:',i4,'  ',a,i7,3x,a)
		if (cmdverbose(-1) <= 1) write (ttyout,*) ' '
		call namdev (idevb, inamr)

		ndevr(imat) = idevb
		nrrec(imat) = ifilb
		mtitle(imat) = ititl
	}
	call wjfren(i,x1,ic2)  # check for delete points indicator
	if (ic2 == ihd) {    # want to delete points
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'DEBUG: deleted point flag set'
		chdltflg='d'
		ic2=0
	}

# write history
	itmp2=lnb(altuse)
	write (lunhist,3050) altuse(1:itmp2), idevb, ifilb,
				chdltflg, ihbcksl,
				 imat, ihbcksl, ititl
3050	format (a,3x,a1,i6,1x,a,15x,a,
		'# file ID, rec no. for material',i5,/,
		a,'#',11('=-'),' TITLE=',a)

	if (inumalt < numaltlib) go to 69   # need to get more alt lib lines

###########################################################
# delete points

3051	if (chdltflg == 'd') {
		if (cmdverbose(-1) <= 1) {
			write (ttyout,*) 'Enter channels to delete, then a c to continue'
		}
		call crtin
		i=1
		call dltpts(i,jdlt,idlt,nchans,ic2)
		if (ic2==ihx)  {
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (ic2!=ihe & jdlt > 1) {
			mxstrdel=240   # 3 lines of 80 characters
			call delhist(jdlt,idlt,mhist,mxstrdel)

			write (lunhist,3055) mhist(1:lnb(mhist(1:80)))
3055			format (a)
			if (mhist(81:81) != ' ') {
				write (lunhist,3055) mhist(81:80+lnb(mhist(81:160)))
			}
			if (mhist(161:161) != ' ') {
				write (lunhist,3055) mhist(161:160+lnb(mhist(161:240)))
			}
			do jj = 1, jdlt {               # delete the channels
				data(idlt(jj))=-1.23e34
			}
		} else {
			write (lunhist,3055) 'c'
		}

	} else {
		write (ttyout,*) 'DEBUG: NOTE: no deleted point flag'
	}

###########################################################
# Title for material "imat"

161	if (cmdverbose(-1) <= 1) write (ttyout,162)
162	format (' Type in an output title',/,
		'-------------------------------|')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 161    # blank line, read again
	if (i > 1) i = i-1
	jend=lnb(iopcon)
	length = jend - i + 1
	if (length > 32) length = 32
	jend = i + length - 1
	otitle(imat)(1:length) = iopcon(i:jend)
	do ij = 1, 32 {
		if(otitle(imat)(ij:ij) == char(0)) {
			otitle(imat)(ij:ij) = ' '
		}
	}

# write history
	write (lunhist, 3060) otitle(imat)(1:32),ihbcksl
3060	format (a, 2x,a,'# output title')


###########################################################
220	if (cmdverbose(-1) <= 1) write (ttyout,225) imat
225	format ('Enter the number of regular features to map for material',i4,/,
		'      followed by the number of "NOT" features')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 220    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il != 0) {
		call what(i)
		go to 220
	}
	if (x < 1 || x > maxfeat) {
		write (ttyout,226) maxfeat
226		format ('ERROR: Number of features per material must be',
			' > 1 and < ',i4)
		go to 220
	}
	nfeat(imat) = x+0.5

	numnotfeat(imat) = 0  # initially set to 0

	call wjfren (i,x,il)
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il != 0) {
		call what(i)
		go to 220
	}
	if (x < 0 || x > maxnotfeat) {
		write (ttyout,227) maxnotfeat
227		format ('ERROR: Number of NOT features per material must be',
			' >= 0 and < ',i4)
		go to 220
	}
	numnotfeat(imat) = x+0.5

	if (ialgorithm(imat) == 1 & (nfeat(imat) > 1 |
			numnotfeat(imat) > 0)) {

		write (ttyout,*) 'ERROR: algorithm "nvres" can only have',
				' one feature, and zero NOT features'
		call what(i)
		go to 220
	}

# write history
	write (lunhist, 3080) nfeat(imat), numnotfeat(imat), ihbcksl
3080	format (i5,3x,i5,20x,a,'# Number of features and not feats')

#	continuum points

297   ndl = 0

  dlbar(imat) = 0.0
  do ifeat = 1, nfeat(imat) {

###########################################################
# RED 07/07/2008 Added a comma between "'  feature',i3" and ':' in the 300 statement
298	if (cmdverbose(-1) <= 1) write (ttyout,300) imat, ifeat
300	format (1x, 'LEFT and RIGHT CONTINUUM INTERVALS for material',
			i4,'  feature',i3,':',//,
		'Enter the feature importance:',/,
		10x,'D  =  Diagnostic feature (must be present)',/,
		10x,'O  =  Optionally present feature',/,
		10x,'W  =  Weak feature',/,
		10x,'      (There is a speed advantage to listing',/,
		10x,'       diagnostic features first.)',/,
		'NEXT Enter two channel numbers to describe the continuum',
			' interval on the ',/,5x,
			'LEFT side of the absorption, and',//,
		'NEXT Enter two channel numbers to describe ',
			'the continuum interval on the ',/,5x,
			'RIGHT side of the absorption ',//,
		' (enter imortance and 4 channel numbers total),',
					 ' then OPTIONS:',/)

	if (cmdverbose(-1) <= 1) write (ttyout,301)
301	format (' Options:',/,
	'         ct n m  where  ct means continuum threshold',/,
	'        lct n m  where lct means left  continuum threshold',/,
	'        rct n m  where rct means right continuum threshold',/,
        '        lcbbrc = (lc-bb)/(rc-bb), bb= band bottom',/,
        '        rcbblc = (rc-bb)/(lc-bb), bb= band bottom',/,
        '        rcbblc, lcbbrc = left contin, band bottom, ',
				'right continuum shape',/,
	17x, 'n is lower, and (optional) m is upper.',/,
	17x, 'Values exceeding this range will reject that material',/,
	17x, '       default = 0.1e-6 to 0.2e+20')

	if (cmdverbose(-1) <= 1) write (ttyout,304)
304	format (/,
	'        lct/rct> n1 n2 where n1 n2 are positive real numbers',/,
	'        rct/lct> n1 n2 where n1 n2 are positive real numbers',/,
	'        rcbblc> n1 n2  where n1 n2 are positive real numbers',/,
	'        rcbblc< n1 n2  where n1 n2 are positive real numbers',/,
	'        lcbbrc> n1 n2  where n1 n2 are positive real numbers',/,
	'        lcbbrc< n1 n2  where n1 n2 are positive real numbers',/,
	'        r*bd>   n1 n2  where n1 n2 are positive real numbers',/,
	'                       note: bd is abs(bd) for positive features',/,
	17x, 'Values < (or >) these ranges will reject that material',//,
	17x, '       default = 0 (no lct/rct of rct/lct checking')

	call crtin
	i=1

	call wjfren (i,x,il)        # feature importance
	if (i >= 80 & il == 0) go to 298    # blank line, read again
	if (il==ihx || il==ihe) {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (il==ihcd) {
		featimprt(ifeat,imat) = 2
	} else if (il==ihco) {
		featimprt(ifeat,imat) = 0
	} else if (il==ihcw) {
		featimprt(ifeat,imat) = 1

	} else {
		call what(i)
		write (ttyout, 302)
302		format (' ERROR: you must first enter FEATURE IMPORTANCE',
			/,' Press return to try again',//)
		call crtin
		go to 298
	}

	call wjfren (i,x,il)        # get channel 1
	if (il == ihw) {
		icmode = 1
	} else {
		icmode = 0
	}

	if (icmode == 1) { # values in wavelengths
		call wjfren (i,x,il)      # get wav1
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
311			format ('ERROR: WAVELENGTH OUT OF RANGE', //)
			go to 298
		}
		wav1 = x

		call wjfren (i,x,il)      # get wav2
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			go to 298
		}
		wav2 = x

		call wjfren (i,x,il)      # get wav3
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			go to 298
		}
		wav3 = x

		call wjfren (i,x,il)      # get wav4
		if (il != 0 || x < 0.1e-30) {
			call what(i)
			write (ttyout, 311)
			go to 298
		}
		wav4 = x
		if (il == ihi) i = i -1  # i set for imaging mode (gotten later)

		call wtochbin (dataa, nchans, wav1, wav2, nch1, nch2, ier)
		if (ier != 0) go to 298

		call wtochbin (dataa, nchans, wav3, wav4, nch3, nch4, ier)
		if (ier != 0) go to 298

		if (cmdverbose(-1) <= 1) {
			write (ttyout,309) wav1, wav2, wav3, wav4,
                                   nch1, nch2, nch3, nch4
309			format (' NOTE: Wavelengths      ',
				3(f11.5,4x),f11.5,/,
	                        '  translate to channels:',3(i6,9x),i6,/)
		}

	} else {
	
		nch1=x
		if (nch1 < 1 || nch1 > nchans) {
			call what(i)
			write (ttyout, 310)
310			format ('ERROR: CHANNEL OUT OF RANGE', //)
			go to 298
		}
		call wjfren (i,x,il)      # get channel 2
		nch2=x
		if (nch2 < 1 || nch2 > nchans) {
			call what(i)
			write (ttyout, 310)
			go to 298
		}
		call wjfren (i,x,il)      # get channel 3
		nch3=x
		if (nch3 < 1 || nch3 > nchans) {
			call what(i)
			write (ttyout, 310)
			go to 298
		}
		call wjfren (i,x,il)      # get channel 4
		nch4=x
		if (nch4 < 1 || nch4 > nchans) {
			call what(i)
			write (ttyout, 310)
			go to 298
		}
		if (il == ihi) i = i -1  # i set for imaging mode (gotten later)
	}

	if (nch1 > nch2 || nch2+1 >= nch3 || nch3 > nch4) {
		write (ttyout,312)
312		format ('ERROR: channels are not in a valid sequence:',/,
			'       If channels = n1 n2 n3 n4, then the following',
					' must hold:',/,
			'       n1 <= n2, n2+1 < n3, and n3 <= n4',//,
			' REENTER ALL POINTS',//)
		go to 298
	}
	cchans(1,ifeat,imat) = nch1
	cchans(2,ifeat,imat) = nch2
	cchans(3,ifeat,imat) = nch3
	cchans(4,ifeat,imat) = nch4
	if (nch1 < chfirst) chfirst = nch1
	if (nch4 > chlast ) chlast  = nch4

#	set default continuaa limits

	zcontmn(ifeat,imat) = 0.1e-6
	zcontlmn(ifeat,imat) = 0.1e-6
	zcontrmn(ifeat,imat) = 0.1e-6
	zcontmx(ifeat,imat) = 0.1e+20
	zcontlmx(ifeat,imat) = 0.1e+20
	zcontrmx(ifeat,imat) = 0.1e+20
	zcontlgtr(1,ifeat,imat) = 0.0
	zcontlgtr(2,ifeat,imat) = 0.0
	zcontrgtl(1,ifeat,imat) = 0.0
	zcontrgtl(2,ifeat,imat) = 0.0

	zrcbblcgt(1,ifeat,imat) = -0.1e+9
	zrcbblcgt(2,ifeat,imat) = -0.2e+9
	zrcbblclt(1,ifeat,imat) =  0.1e+9
	zrcbblclt(2,ifeat,imat) =  0.2e+9
	zlcbbrcgt(1,ifeat,imat) = -0.1e+9
	zlcbbrcgt(2,ifeat,imat) = -0.2e+9
        zlcbbrclt(1,ifeat,imat) =  0.1e+9
        zlcbbrclt(2,ifeat,imat) =  0.2e+9

	zrtimesbd(1,ifeat,imat) =  0.1e-9
	zrtimesbd(2,ifeat,imat) =  0.1e-9

	ictlimflg = 0
	ictrlimflg = 0
	ictllimflg = 0
	ictlgtrflg = 0
	ictrgtlflg = 0
	ircbblcgtflg = 0
	ircbblcltflg = 0
	ilcbbrcgtflg = 0
	ilcbbrcltflg = 0

313	call wjfren (i,x,il)      # check if continuum limits set.
                                  # loop back to here until all options found

	if ((i < 72) & (il == ihl) & 
			(iopcon(i:i+6) == 'ct/rct>')) {  # lct/rct> n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding lct/rct>'
			go to 298
		}
		zcontlgtr(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding lct/rct>'
			go to 298
		}
		ictlgtrflg = 1
		zcontlgtr(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: lct/rct>', x, x2,' ifeat=',ifeat,' material=',imat
	}
	if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+6) == 'ct/lct>')) {  # rct/rcl> n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rct/lct>'
			go to 298
		}
		zcontrgtl(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rct/lct>'
			go to 298
		}
		ictrgtlflg = 1
		zcontrgtl(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: rct/lct>', x, x2,' ifeat=',ifeat,' material=',imat
	}
	if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+5) == 'cbblc>')) {  # rcbblc> n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			go to 298
		}
		zrcbblcgt(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			go to 298
		}
		ircbblcgtflg = 1
		zrcbblcgt(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: rcbblc>', x, x2,' ifeat=',ifeat,' material=',imat
	}
	if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+5) == 'cbblc<')) {  # rcbblc< n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc<'
			go to 298
		}
		zrcbblclt(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc<'
			go to 298
		}
		ircbblcltflg = 1
		zrcbblclt(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: rcbblc<', x, x2,' ifeat=',ifeat,' material=',imat
	}
	if ((i < 72) & (il == ihl) & 
			(iopcon(i:i+5) == 'cbbrc>')) {  # rcbblc> n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			go to 298
		}
		zlcbbrcgt(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			go to 298
		}
		ilcbbrcgtflg = 1
		zlcbbrcgt(2,ifeat,imat) = x2
		#write (ttyout,*) 'DEBUG: lcbbrc>', x, x2,' ifeat=',ifeat,' material=',imat
	}
	if ((i < 72) & (il == ihl) & 
			(iopcon(i:i+5) == 'cbbrc<')) {  # rcbblc< n option found
		i = i+7
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			go to 298
		}
		zlcbbrclt(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rcbblc>'
			go to 298
		}
		ilcbbrcltflg = 1
		zlcbbrclt(2,ifeat,imat) = x2
		write (ttyout,*) 'DEBUG: lcbbrc>', x, x2,' ifeat=',ifeat,' material=',imat
	}
	if ((i < 72) & (il == ihr) & 
			(iopcon(i:i+3) == '*bd>')) {  # r*bd> n m option found
		i = i+4
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding r*bd>'
			go to 298
		}
		zrtimesbd(1,ifeat,imat) = x
		call wjfren (i,x2,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding r*bd>'
			go to 298
		}
		ilcbbrcltflg = 1
		zrtimesbd(2,ifeat,imat) = x2
		write (ttyout,*) 'DEBUG: r*bd>', x, x2,' ifeat=',ifeat,' material=',imat
	}
	
	if ((i < 80) & (il == ihc) & (iopcon(i:i) == 't')) {
		  # ct option found
		i = i+1
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding ct min max'
			go to 298
		}
		ictlimflg = 1
		zcontmn(ifeat,imat) = x

		call wjfren (i,x,il)  # now see if max continuum is set.
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0 & il != ihr & il != ihl) {
			call what (-1)
			write (ttyout,*) 'ERROR decoding ct max'
			go to 298
		}
		if (il != 0 & (il == ihr | il == ihl)) {
			i = i -1
			go to 313   # found another option, no max
		}
		if (i < 80 & il != ihr & il != ihl) {
			if (x <= zcontmn(ifeat,imat)) {
				call what(i)
				write (ttyout,*) 'ERROR: max is less than min'
				go to 298
			}
			ictlimflg = 2
			zcontmx(ifeat,imat) = x
		}
	} else  if ((i < 79) & (il == ihr) &
			(iopcon(i:i) == 'c') & 
				(iopcon(i+1:i+1) == 't')) {  # rct option found
		i = i+2
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding rct min max'
			go to 298
		}
		ictrlimflg = 1
		zcontrmn(ifeat,imat) = x

		call wjfren (i,x,il)  # now see if max continuum is set.
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0 & il != ihc & il != ihl) {
			call what (-1)
			write (ttyout,*) 'ERROR decoding rct max'
			go to 298
		}
		if (il != 0 & (il == ihr | il == ihl)) {
			i = i -1
			go to 313   # found another option, no max
		}
		if (i < 80 & il != ihr & il != ihl) {
			if (x <= zcontrmn(ifeat,imat)) {
				call what(i)
				write (ttyout,*) 'ERROR 2 decoding rct max'
				go to 298
			}
			ictrlimflg = 2
			zcontrmx(ifeat,imat) = x
		}
	} else if ((i < 79) & (il == ihl) &
			(iopcon(i:i) == 'c') & 
				(iopcon(i+1:i+1) == 't')) {  # lct option found
		i = i+2
		call wjfren (i,x,il)
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0) {
			call what(i)
			write (ttyout,*) 'ERROR decoding lct min max'
			go to 298
		}
		ictllimflg = 1
		zcontlmn(ifeat,imat) = x

		call wjfren (i,x,il)  # now see if max continuum is set.
		if (il==ihx || il==ihe) {
			ic=il
			icrst = 1
			call rstart(icrst)
			call what (-1)
			return
		}
		if (il != 0 & il != ihc & il != ihl) {
			call what (-1)
			write (ttyout,*) 'ERROR decoding lct max'
			go to 298
		}
		if (il != 0 & (il == ihr | il == ihl)) {
			i = i -1
			go to 313   # found another option, no max
		}
		if (i < 80 & il != ihr & il != ihl) {
			if (x <= zcontlmn(ifeat,imat)) {
				call what(i)
				write (ttyout,*) 'ERROR 2 decoding lct max'
				go to 298
			}
			ictllimflg = 2
			zcontlmx(ifeat,imat) = x
		}
	}

	if (i < 80) go to 313   # loop through all possible options

# write history
	chtmp1 = ' '   # initialize temp array
	ichtmp1 = 1
	ichtmplen = len(chtmp1)

	if (ictlimflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3097) 'ct',
					zcontmn(ifeat,imat)
3097		format (' ',a,f8.4)
		ichtmp1 = ichtmp1 + 11
	} else if (ictlimflg == 2) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'ct',
					zcontmn(ifeat,imat),
					zcontmx(ifeat,imat)
3098		format (' ',a,f8.4,1x,f8.4)
		ichtmp1 = ichtmp1 + 20
	}
	if (ictrlimflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3097) 'rct',
					zcontrmn(ifeat,imat)
		ichtmp1 = ichtmp1 + 12
	} else if (ictrlimflg == 2) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rct',
					zcontrmn(ifeat,imat),
					zcontrmx(ifeat,imat)
		ichtmp1 = ichtmp1 + 21
	}
	if (ictllimflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3097) 'lct',
					zcontlmn(ifeat,imat)
		ichtmp1 = ichtmp1 + 12
	} else if (ictrlimflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rct',
					zcontlmn(ifeat,imat),
					zcontlmx(ifeat,imat)
		ichtmp1 = ichtmp1 + 21
	}
	if (ictlgtrflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'lct/rct>',
			zcontlgtr(1,ifeat,imat), zcontlgtr(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
	if (ictrgtlflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rct/lct>',
			zcontrgtl(1,ifeat,imat), zcontrgtl(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
        if (ircbblcgtflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rcbblc> ',
			zrcbblcgt(1,ifeat,imat), zrcbblcgt(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
        if (ircbblcltflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'rcbblc< ',
			zrcbblclt(1,ifeat,imat), zrcbblclt(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
        if (ilcbbrcgtflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'lcbbrc> ',
			zlcbbrcgt(1,ifeat,imat), zlcbbrcgt(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
        if (ilcbbrcltflg == 1) {
		write (chtmp1(ichtmp1:ichtmplen), 3098) 'lcbbrc< ',
			zlcbbrclt(1,ifeat,imat), zlcbbrclt(2,ifeat,imat)
		ichtmp1 = ichtmp1 + 18
	}
	#
	# now compress blanks
	#
	chtmp2 = ' '
	chtmp2(1:1) = chtmp1(1:1)
	itmp2 = 1
	do itmp1 = 2, ichtmplen {

		if (chtmp1(itmp1:itmp1) != ' ') {
			chtmp2(itmp2:itmp2) = chtmp1(itmp1:itmp1)
			itmp2 = itmp2 + 1
		} else if (chtmp1(itmp1-1:itmp1-1) == ' ' & 
				chtmp1(itmp1:itmp1)  == ' ') {
				next
		} else {
			chtmp2(itmp2:itmp2) = chtmp1(itmp1:itmp1)
			itmp2 = itmp2 + 1
		}
	}

	if (icmode == 1) { # values in wavelengths
		write (lunhist, 3091) imch(featimprt(ifeat,imat)+1),
					wav1, wav2, wav3, wav4,
					chtmp2(1:itmp2), ihbcksl,
					ihbcksl, nch1, nch2, nch3, nch4
3091		format (a,'w',4(f8.4,1x),a,1x,a,
				'# continuum wavelengths',
				/,a,'#',31x,'(continuum channels=)',
				4(i4,' '))
	} else {
		write (lunhist, 3094) imch(featimprt(ifeat,imat)+1),
					nch1, nch2, nch3, nch4,
					chtmp2(1:itmp2),
					ihbcksl, imat, ifeat,
					ihbcksl, dataa(nch1), dataa(nch2),
					dataa(nch3), dataa(nch4)
3094		format (a,4(i5,' '),1x,a,2x,a,'# continuum chanls for Mat.:',
				i3,' Feat:',i3,/,
				a,'#',33x, a,'waves:',4(f7.4,' '))
	}


###########################################################
#     parameter description for bdm set call:
#        INPUT:
#           dataa = wavelengths
#           data  = reference library spectrum  R*4 (nch4 elements)
#           nch1   = continuum point begin on left side of band  I*4
#           nch2    = continuum point end on left side of band  I*4
#                      note: nch2 >= nch1  (checked)
#           nch3    = continuum point begin on right side of band  I*4
#                      note: nch3 > nch2 + 1 (checked)
#           nch4    = continuum point end on right side of band  I*4
#                      note: nch4 >= nch3 (checked)
#                      note: nch4 also determines the max array sizes
#        OUTPUT:
#           refcrm = reference library spectrum, continuum
#                                    removed  R*4 (nch4 elements)
#           minch  = minimum channel in the reference library spectrum.
#                    This is defined to be the band minimum.
#           maxch  = maximum channel in the reference library spectrum.
#                    This is defined to be the band maximum.
#           ifeattype = -1 is an emission feature
#                     =  1 is an absorption band
#           ier    = error detected in input:
#                       = 0 no error
#                       = 1 error
#   

	do i = 1, nchans {
		rlb(i,imat) = data(i)
		rlbc(i,ifeat,imat) = 0.0
	}

	call bdmset (dataa,data,nch1,nch2,nch3,nch4,
                           refcrm,minch,maxch,ifeattype,ier)

	if (ier != 0) {
		write (ttyout, 320)
320		format ('ERROR in band mapping library setup',/,
			'Press return to start over',///)
		call what (-1)
		go to 210
	}

	do i = nch1, nch4 {
		rlbc(i,ifeat,imat) = refcrm(i)
	}
	nchmin(ifeat,imat)= minch
	nchmax(ifeat,imat)= maxch
	nftype(ifeat,imat)= ifeattype
	if (ifeattype == -1) {
		if (cmdverbose(-1) <= 1) {
			write (ttyout,*) 'Emisssion feature ',
					'maximum channel=', maxch
		}
	} else if (ifeattype == 1) {
		if (cmdverbose(-1) <= 1) {
			write (ttyout,*) 'Absorption feature ',
					'maximum channel=', minch
		}
	} else {
		write (ttyout,*) 'ERROR: invalid feature type'
		write (ttyout,*) ' '
		call what (-1)
		go to 298
	}
	nsp1 = nch2 +1  # first point in band not part of continuum
	nsp2 = nch3 -1  # last point in band not part of continuum
	area = 0.0
	do i = nsp1, nsp2 {
		if (refcrm(i) != -1.23e34) {
			area = area + abs(1.0 - refcrm(i)) # integrate absolute
							# area under contin
		}
	}
	if (area <= 0.0) {
		write (ttyout, 322) area, ifeat, imat
322		format (' ERROR: area of continuum removed',
			' absorption is < 0:',1pe10.4,' ifeat:',
			i4,' imat:',i4)
	}
	if (featimprt(ifeat,imat) != 1) {  # do not count weak features
		dl(ifeat,imat) = area
		dlbar(imat) = dlbar(imat) + dl(ifeat,imat)
		ndl = ndl + 1
	} else {
		dl(ifeat,imat) = 0.0  # weak feature: set area to zero
				      # even though it is finite.  This
				      # effectively takes it out of any
				      # calculations.
	}

  }  # end of do ifeat loop

###########################################################
  if (numnotfeat(imat) > 0) {   # Get "NOT" features

      do infeat = 1, numnotfeat(imat) {

	notfrn(infeat,imat) = 0  # initialize

305	if (cmdverbose(-1) <= 1) write (ttyout,303)
303	format (//,'NOT Features:',//,
		'  Enter NOT followed by the File ID, Rec. No., ',
				'feature #, threshold',
					' depth[a,r#] and threshold fit',/,
		'         where a  = absolute depth',/,
		'               r# = relative depth, so that',/,
		'                    NOT feat depth / r# depth is tested',/,
		'                    where # = a feature from the current',/,
                '                    material',/,
		'  Examples:',/,
		'     NOT [sprln01] 12 2 0.05a  0.3 \# absolute depth .05, fit .3',/,
		'     NOT [sprln01] 12 2 0.05r2 0.3 \# relative depth .05, fit .3',//,
		'  File ID and Rec No. must be an previosly entered',
			' spectrum')
		
	call crtin
	i = 1
	call wjfren(i,x1,il)
	if (i >= 80 & il == 0) go to 305    # blank line, read again
#        *** check for hard or soft exit ***
	if (il == ihx || il == ihe) {
		ic = il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
#	                   check for NOT key word
	if (il != ihcn) {
		call what (i)
		write (ttyout,*) 'NOT key word not found'
		go to 69
	}
	call wjfren(i,x1,il)
	if (il != ihco) {
		call what (i)
		write (ttyout,*) 'NOT key word not found'
		go to 69
	}
	call wjfren(i,x1,il)
	if (il != ihct) {
		call what (i)
		write (ttyout,*) 'NOT key word not found'
		go to 69
	}
	call wjfren(i,x1,idevb)
	if (i >= 79 && idevb == 0) {
		call what(i)
		go to 69
	}
	call wjfren(i,xfilb,ic2)

#       *** check for invalid input ***
	if (x1!=0.0 || ic2!=0 || xfilb<=0.0) {
		write(ttyout,66)
		go to 69

#       *** looks ok so get which material ***
	} else {
		if (imat > 1) {
			notfid(infeat,imat) = 0
			do intmp = 1, imat -1 {
				###write (6,*) 'DEBUG:',intmp,ndevr(intmp),nrrec(intmp),'=?',idevb,xfilb
				if (ndevr(intmp) == idevb &
						nrrec(intmp) == xfilb) {	
					notfid(infeat,imat) = idevb
					notrec(infeat,imat) = xfilb
					notmat(infeat,imat) = intmp
				}
			}
			if (notfid(infeat,imat) == 0) {
				call what(-1)
				write (ttyout,*) 'ERROR: NOT feature not found:',
							notfid(infeat,imat),
							notrec(infeat,imat)
				go to 69
			}
		} else {
			call what(-1)
			write (ttyout,*) 'ERROR: can not have NOT features with material 1'
			go to 69
		}

	}

	call wjfren(i,x, il)  # get feature number
	if (nint(x) <= nfeat(notmat(infeat,imat)) ) {   # check if NOT feature number
						#  is valid
		notfeat(infeat,imat) = x + 0.5
	} else {
		call what(-1)
		write (ttyout,*) 'ERROR: ',nint(x),'is not a valid feature in',
					'material ',notmat(infeat,imat),
					' which has ',
					nfeat(notmat(infeat,imat)),' features'
		go to 69
	}
	thrshdnot(infeat,imat) = 1.0
	call wjfren(i,x, il)  # get threshold depth
	if (x > 0.0 & x < 20.0 & (il == iha | il == ihr)) {
		thrshdnot(infeat,imat) = x
	} else {
		call what(i)
		write (ttyout,*) "threshold NOT depth out of range (>0, <20)"
		write (ttyout,*) "    or no [r] or [a] flag"
		go to 69
	}
	if (il == iha) {   # depth is absolute number

		notfrn(infeat,imat) = 0

	} else if (il == ihr) {   # depth is relative, so get what its relative to

		call wjfren(i,x, il)
		if (x > 0.0 & nint(x) <= nfeat(imat) & il == 0) {

			notfrn(infeat,imat) = nint(x)

		} else {
			call what(i)
			write (ttyout,*) "NOT relative feature invalid"
			go to 69
		}
	} else {
		call what(i)
                write (ttyout,*) "NOT feature: no [r] or [a] flag"
                go to 69
	}

	thrshfnot(infeat,imat) = 1.0
	call wjfren(i,x, il)  # get threshold fit
	if (x > 0.0 & x < 1.0 & il == 0) {
		thrshfnot(infeat,imat) = x
	} else {
		call what(i)
		write (ttyout,*) threshold NOT fit out of range
		go to 69
	}
	if (notfrn(infeat,imat) == 0) {            # absolute feat depth history
		write (lunhist, 3031) notfid(infeat,imat),
				notrec(infeat,imat),
				notfeat(infeat,imat),
				thrshdnot(infeat,imat),
				thrshfnot(infeat,imat),  ihbcksl,
				notmat(infeat,imat)
	} else {
		write (lunhist, 3033) notfid(infeat,imat),
				notrec(infeat,imat),
				notfeat(infeat,imat),
				thrshdnot(infeat,imat),
				notfrn(infeat,imat),
				thrshfnot(infeat,imat),  ihbcksl,
				notmat(infeat,imat)
	}
3031	format ('NOT ',a1,i6,2x,i3,2x,f7.4,'a',1x,f7.4,5x,a,
		'# NOT ID, rec, (mat=',i5,') feat, thrsh depth, thrsh fit')
3033	format ('NOT ',a1,i6,2x,i3,2x,f7.4,'r',i2,1x,f7.4,5x,a,
		'# NOT ID, rec, (mat=',i5,') feat, thrsh depth, thrsh fit')

	if (nftype(notfeat(infeat,imat),notmat(infeat,imat)) == 1) {
							# absorption band
		wavtmp = dataa(nchmin(notfeat(infeat,imat),notmat(infeat,imat)))
	} else {					# emission band
		wavtmp = dataa(nchmax(notfeat(infeat,imat),notmat(infeat,imat)))
	}
	write (lunhist, 3032) ihbcksl, wavtmp, mtitle(notmat(infeat,imat))
3032	format (a,'#  NOT feature at wavelength',f7.3,' for:',a)

     }
  }

  if (ndl < 1) {
	write (ttyout,*) 'ERROR: no diagnostic or optional features ',
		'declared, you must have at leat one'
	i =1
	call what(i)
	go to 297
  }
  dlsum(imat) = dlbar(imat)             # this is the summed ref lib band depth areas
  dlbar(imat) = dlbar(imat)/ float(ndl) # this is the normalized summed areas

###########################################################
  do ifeat = 1, nfeat(imat) {   # compute normalized lib band depths

	dln(ifeat,imat) = dl(ifeat,imat)/dlsum(imat)

#       history notes:
	if (ifeat == 1) write (lunhist,3101) ihbcksl
	write (lunhist, 3100) ihbcksl, ifeat, dln(ifeat,imat)
3100	format (a,'#',9x,'Feat:',i3,
		'  has a weight of',f6.3)
3101	format (a,'# Notes:')
  }
  

###########################################################
#       now check for fit, dpeth, fd thresholds

7	thrshfit(1,imat)     = 0.0  # thresholds
 	thrshfit(2,imat)     = 0.0  # thresholds
	thrshdepth(1,imat)   = 0.0
	thrshdepth(2,imat)   = 0.0
	thrshfd(1,imat)      = 0.0
	thrshfd(2,imat)      = 0.0
	thrshfdfit(1,imat)   = 0.0
	thrshfdfit(2,imat)   = 0.0
	thrshfddepth(1,imat) = 0.0
	thrshfddepth(2,imat) = 0.0
	thrshfitall(1,imat)  = 0.0
	thrshfitall(2,imat)  = 0.0
	thrshdepthall(1,imat)= 0.0
	thrshdepthall(2,imat)= 0.0
	thrshdepthfit(1,imat)= 0.0
	thrshdepthfit(2,imat)= 0.0
	thrshfdall(1,imat)   = 0.0
	thrshfdall(2,imat)   = 0.0

# set other constraints

	mtemp(1,imat)   = -999.   # internal units in centigrade
	mtemp(2,imat)   = -999.
	mtemp(3,imat)   = -999.
	mtemp(4,imat)   = -999.

        mpressure(1,imat)   = -999.   # units in bars
        mpressure(2,imat)   = -999.   # units in bars
        mpressure(3,imat)   = -999.   # units in bars
        mpressure(4,imat)   = -999.   # units in bars

	mclass(imat) = 1   # default class is 1st class
	dclass(imat) = 1.0 # class difference for action (1= no rejection)

	nfeatratio(imat) = 0          # zero feature ratios
	do itmpfr = 1, maxfeatratio {
		featidratio (1,itmpfr,imat) = 0
		featidratio (2,itmpfr,imat) = 0
		featratio(1,itmpfr,imat)    = 0.0
		featratio(2,itmpfr,imat)    = 0.0
		featratio(3,itmpfr,imat)    = 0.0
		featratio(4,itmpfr,imat)    = 0.0
	}

466	if (cmdverbose(-1) <= 1) write (ttyout,467) imat, mtitle(imat)
	if (cmdverbose(-1) <= 1) write (ttyout,4671)
	if (cmdverbose(-1) <= 1) write (ttyout,468)
467	format(///,
	' Material',i3,': ',a,/,
	' Options: enter:  Default = 1.0.',//,10x,
	'constraint: FIT>       fit   threshold, apply to fit   image',/,10x,
	'constraint: FITALL>    fit   threshold, apply to all   images',/,10x,
	'constraint: DEPTH>     depth threshold, apply to depth image',/,10x,
	'constraint: DEPTHALL>  depth threshold, apply to all   images',/,10x,
	'constraint: DEPTH-FIT> fit   threshold, apply to depth image',/,10x,
	'constraint: FD>        f*d   threshold, apply to f*d   image',/,10x,
	'constraint: FDALL>     f*d   threshold, apply to all   images',/,10x,
	'constraint: FD-FIT>    fit   threshold, apply to f*d   image',/,10x,
	'constraint: FD-DEPTH>  depth threshold, apply to f*d   image')
4671	format (/,
	'constraint: fratio: a / b = r1 r2 r3 r4  where a, b = feature number',
	'                            r1, r2=min; r3, r4=max with fuzzy logic')

468     format(/,10x,
	'constraint: temperature t1 t2 t3 t4 ',/,10x,
	'constraint: pressure    p1 p2 p3 p4 ',/,10x,
	'constraint: class       n  classdiff',//,
	'each fit, depth, f*d is 2 values: min-max fuzzy logic levels',/,
		10x,' Example:   constraint: FIT> 0.2 0.4 DEPTH> 0.004 0.006',/,
		10x,' Example:   constraint: temperature -10 0 100 110 ',/,
		10x,' Example:   constraint: class 2 0.02 ')

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 466    # blank line, read again
	if (il==ihx )  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	ifirst= fnb(iopcon) # find first non blank
	if (ifirst < 1) ifirst = 1
	i = ifirst - 1
	ilast= lnb(iopcon)  # find last non blank
	if (ilast > 80) ilast = 80
	if (ilast < 1) ilast = 1
	print *,"i :",i
	if (iopcon(i+1:i+11) == 'constraint:') {
		go to 3103   # constraint found, continue.

	} else if (iopcon(i+1:i+13) == 'endconstraint') {
		
		write (lunhist, *) 'endconstraint'

		go to 474   # next imput line
	} else {

		write (ttyout,*) 'ERROR: expecting constraint or ',
				'endconstraint key word'
		write (ttyout,*) 'input line:'
		write (ttyout,*) iopcon
		call what(i)
		go to 466
	}
	
3103	i = i + 1
	if (i >= 70) go to 3106  #beyond 70 has no room for parameters
				 # so write history for this line
	if (iopcon(i:i) == ' ') go to 3103

	###write (ttyout,*) 'DEBUG: i, iopcon:',i, iopcon(i:i+9)

	if (iopcon(i:i+3) == 'FIT>') {
		i = i + 4
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfit(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfit=',thrshfit(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfit(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfit=',thrshfit(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+6) == 'FITALL>') {
		i = i + 7
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfitall(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfitall=',thrshfitall(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfitall(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfitall=',thrshfitall(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+5) == 'DEPTH>') {
		i = i + 6
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshdepth(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepth=',thrshdepth(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshdepth(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepth=',thrshdepth(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+8) == 'DEPTHALL>') {
		i = i + 9
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshdepthall(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepthall=',thrshdepthall(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshdepthall(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepthall=',thrshdepthall(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+9) == 'DEPTH-FIT>') {
		i = i + 10
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshdepthfit(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepthfit=',thrshdepthfit(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshdepthfit(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshdepthfit=',thrshdepthfit(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+2) == 'FD>') {
		i = i + 3
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfd(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfd=',thrshfd(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfd(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfd=',thrshfd(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+5) == 'FDALL>') {
		i = i + 6
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfdall(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfdall=',thrshfdall(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfdall(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfdall=',thrshfdall(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+6) == 'FD-FIT>') {
		i = i + 7
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfdfit(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfdfit=',thrshfdfit(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfdfit(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfdfit=',thrshfdfit(2,imat)
			go to 3103
		}
	}
	if (iopcon(i:i+8) == 'FD-DEPTH>') {
		i = i + 9
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfddepth(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',thrshfddepth(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			thrshfddepth(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',thrshfddepth(2,imat)
			go to 3103
		}
	}

	#constraint: fratio: a / b = r1 r2 r3 r4  where a, b = feature number
	#                            r1, r2=min; r3, r4=max with fuzzy logic
	#       nfeatratio(imat)
	#	featidratio (1,itmpfr,imat) 
	#	featidratio (2,itmpfr,imat)
	#	featratio(4,itmpfr,imat)

	if (iopcon(i:i+6) == 'fratio:') {
		i = i + 7
		call wjfren (i,x,il)
		if (il != 0) {
			if (il != ihchar('/')) {
				call what(i)
				go to 7
			}
		} else {
			nfeatratio(imat) = nfeatratio(imat) + 1
			nfr = nfeatratio(imat) 
			if (nfr > maxfeatratio) {
				write (ttyout,*) 'ERROR: too many ',
					'feature ratios:', nfr,
					'> max of ', maxfeatratio,
					'material ', imat
				call what(0)
				go to 7
			}
			featidratio(1,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio1=',x

			if (featidratio(1,nfr,imat) >  nfeat(imat) |
				featidratio(1,nfr,imat) < 1) {

				write (ttyout,*) 'ERROR: ',
					'feature ratio 1:', int(x),
					'> features ', nfeat(imat),
					'material ', imat,
					' (or < 0)'
				call what(0)
				go to 7
			}

		}
		call wjfren (i,x,il)
		if (il != 0) {
			if (il != ihchar('=')) {
				call what(i)
				go to 7
			}
		} else {
			featidratio(2,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio2=',x

			if (featidratio(2,nfr,imat) >  nfeat(imat) |
				featidratio(2,nfr,imat) < 1) {

				write (ttyout,*) 'ERROR: ',
					'feature ratio 1:', int(x),
					'> features ', nfeat(imat),
					'material ', imat,
					' (or < 0)'
				call what(0)
				go to 7
			}
		}
		call wjfren (i,x,il)
		if (il == ihchar('=')) {
			call wjfren (i,x,il)
		}
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			featratio(1,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio 1=', x
		}

		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			featratio(2,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio 2=', x

			if (featratio(2,nfr,imat) <= featratio(1,nfr,imat)) {
				write(ttyout,*) 'ERROR: fratio parameter 2<=1'
				call what(0)
				go to 7
			}
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			featratio(3,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio 3=', x

			if (featratio(3,nfr,imat) <= featratio(2,nfr,imat)) {
				write(ttyout,*) 'ERROR: fratio parameter 3<=2'
				call what(0)
				go to 7
			}
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			featratio(4,nfr,imat) = x
			###write (ttyout,*) 'DEBUG:fratio 4=', x

			if (featratio(4,nfr,imat) <= featratio(3,nfr,imat)) {
				write(ttyout,*) 'ERROR: fratio parameter 4<=3'
				call what(0)
				go to 7
			}
			go to 3103
		}
	}

	if (iopcon(i:i+11) == 'temperature:') {
		i = i + 12
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			mtemp(1,imat) = x
			###write (ttyout,*) 'DEBUG:temperature 1=',mtemp(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			mtemp(2,imat) = x
			###write (ttyout,*) 'DEBUG:temperature 2=',mtemp(2,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			mtemp(3,imat) = x
			###write (ttyout,*) 'DEBUG:temperature 3=',mtemp(3,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			mtemp(4,imat) = x
			###write (ttyout,*) 'DEBUG:temperature 4=',mtemp(4,imat)
			go to 3103
		}
	}

	if (iopcon(i:i+8) == 'pressure:') {
		i = i + 9
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			mpressure(1,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mpressure(1,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			mpressure(2,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mpressure(2,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			mpressure(3,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mpressure(3,imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			mpressure(4,imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mpressure(4,imat)
			go to 3103
		}
	}

	if (iopcon(i:i+5) == 'class:') {
		i = i + 6
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			mclass(imat) = int(x+0.5)
			###write (ttyout,*) 'DEBUG:thrshfddepth=',mclass(imat)
		}
		call wjfren (i,x,il)
		if (il != 0) {
			call what(i)
			go to 7
		} else {
			dclass(imat) = x
			###write (ttyout,*) 'DEBUG:thrshfddepth=',dclass(imat)
			go to 3103
		}
	}

        # add additional constraints here

	go to 3103

	
# write history
3106	write (lunhist, 3110) iopcon(ifirst:ilast), ihbcksl
3110	format ('constraint: ',a,5x,a,'# options')

        go to 466   # loop back for more constraint input.

###########################################################
## files to output if imaging

474	if (cmdverbose(-1) <= 1) write (ttyout,475)
475	format(///,'Enter: output= and the images to output ',
					'(when in image mode',/,
		10x,'output  options: fit depth fd none',/,
		10x,'example: output= fit depth fd')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 474    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (index(iopcon,'output=') == 0) {
		call what (-1)
		write (ttyout,*) 'ERROR: output= keyword not found'
		go to 474
	}
	oenblfit(imat) = 0
	oenbldepth(imat) = 0
	oenblfd(imat) = 0
	cht1 = '   '
	cht2 = '     '
	cht3 = '  '
	if (index(iopcon,'fit') > 0) {
		oenblfit(imat) = 1
		cht1 = 'fit'
	}
	if (index(iopcon,'depth') > 0) {
		oenbldepth(imat) = 1
		cht2='depth'
	}
	if (index(iopcon,'fd') > 0) {
		oenblfd(imat) = 1
		cht3='fd'
	}
	if (index(iopcon,'none') > 0) {
		if (cmdverbose(-1) <= 1) write (ttyout,*) 'WARNING: no image output'
	}

	write (lunhist, 476) cht1, cht2, cht3
476	format ('output= ',a,' ',a,' ',a)


###########################################################
# Base file name for material "imat"

180	if (cmdverbose(-1) <= 1) write (ttyout,182)
182	format (///,' Output files for cube analysis: ',/,
		' Type in the output BASE file name',/,
		'      The output files will be called:',/,
		'               filename.depth',/,
		'               filename.fit',/,
		'               filename.fd',/,
		'-------------------------------------------|')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 180    # blank line, read again
	if (i >= 79) {
		call what(i)
		go to 180
	}
	if (i > 1) i = i-1
	jfirst=fnb(iopcon)
	if (jfirst < 1) jfirst = 1
	jend  =lnb(iopcon)
	if (jend > 80) jend = 80
	if (jend < 1) jend = 1
	length = jend - jfirst + 1
	###write (6,*) 'DEBUG: jfirst jend, length',jfirst, jend, length
	if (length > mfilelen) {
		write (ttyout,*) 'WARNING: truncating file name'
		length = mfilelen
	}
	jend = jfirst + length - 1                 #truncate
	###write (6,*) 'DEBUG: jfirst jend, length',jfirst, jend, length
	mfile(imat)(1:length) = iopcon(jfirst:jend)
	itmp = index(mfile(imat)(1:length),' ')    # check for blanks
	if (itmp != 0) {
		call what(jfirst+itmp)
		write (ttyout,184)
184		format (' ERROR: blanks are not allowed in file names',/)
		go to 180
	}
	itmp = index(mfile(imat)(1:length),char(9))  # check for tabs
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,185)
185		format (' ERROR: tabs are not allowed in file names',/)
		go to 180
	}
	do ij = 1, mfilelen {
		if (mfile(imat)(ij:ij) == char(0)) mfile(imat)(ij:ij)=' '
	}

	lenfile(imat)=length

	dfile = mfile(imat)(1:length) // '.depth'
	if (oenblfit(imat) == 1) {
		inquire (file=dfile(1:length+6),exist=fexist)

		if (fexist) {
			write (ttyout,187) ier, 'depth', dfile
187			format (' ERROR',i5,' on ',a,' file:',/,a,
				' FILE EXISTS, but it should not')
			call what (-1)
			go to 180
		}
	}

	ffile = mfile(imat)(1:length) // '.fit'
	if (oenblfit(imat) == 1) {
		inquire (file=ffile(1:length+4), exist=fexist)

		if (ier != 0) {
			write (ttyout,187) ier, 'fit', ffile
			call what (-1)
			go to 180
		}
	}

	fdfile = mfile(imat)(1:length) // '.fd'
	if (oenblfd(imat) == 1) {
		inquire (file=fdfile(1:length+3), exist=fexist)

		if (ier != 0) {
			write (ttyout,187) ier, 'fit*depth', fdfile
			call what (-1)
			go to 180
		}
	}

	if (cmdverbose(-1) <= 1) {
		write (ttyout,*) ' '
		write (ttyout,*) ' '
	}

# write history
	write (lunhist,3070) mfile(imat)(1:mfilelen), ihbcksl
3070	format (a,3x,a,'# Output base file name')

###########################################################
     # Prompt the user for scaling factors for output
	bdscal(imat) = 1.0
	qfscal(imat) = 1.0  # note this is hard set to 1.0

464	if (cmdverbose(-1) <= 1) write (ttyout,465) imat, mtitle(imat)
465	format(///,' Enter the output and scale for the band depth for',//,
		' Material',i3,': ',a,/,
		' Enter output bits/pixel DN dn# = real# option',//,
		5x,'bits/pixel = 8 or 16',/,
		5x,'dn# is an image data number to scale real# to',/,
		5x,'Example:  8     DN 255   = 0.5',/,
		5x,'means 8 bits/pixel and scale 0.5 to a DN of 255')

	call crtin
	i=1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 464    # blank line, read again

	if (il == ihe | il == ihx) {
		icrst = 1
		call rstart (icrst)
		write (ttyout,*)'Exiting.'
		call what (-1)
		return
	} else if (il != 0) {
		call what(i)
		go to 464
	} else {
		itmp = x + 0.5
		if (itmp != 8 & itmp != 16) {
			call what(i)
			write (ttyout,*)'ERROR: bits must be 8 or 16'
			go to 464
		}
		obits(imat) = itmp
	}

	call wjfren (i,x,il)
	if (il == ihcd & iopcon(i:i) == 'N') {    # DN found

		i = i+1
	} else {
		call what(i)
		write (ttyout,*) 'ERROR: DN not found'
		go to 464
	}
	noeql = 0
	call wjfren (i,x,il)
	if (il == 0 | il == ihchar('=')) {

		idn = x + 0.5
		if (obits(imat) == 8 & (idn < 1 | idn > 255)) {
			call what(i-1)
			write (ttyout,*) 'ERROR: dn value out of ',
							'8-bit range'
			go to 464
		} else if (obits(imat) == 16 & 
				(idn < -32767 | idn > 32767 |
							idn == 0)) {
			call what(i-1)
			write (ttyout,*) 'ERROR: dn value out of ',
							'16-bit range'
			go to 464
		}
		if (il == ihchar('=')) noeql = 1
	}
	if (noeql == 0) {           # looking for =
		call wjfren (i,x,il)
		if (il != ihchar('=')) {
			call what(i)
			write (ttyout,*) 'ERROR: = expected'
			go to 464
		}
	}
	call wjfren (i,x,il)
	if (il != 0) {
		call what(i)
		go to 464
	}
	if (abs(x) < 0.01 | abs(x) > 1000.0) {
		
		call what(i-1)
		write (ttyout,*) 'ERROR: value out of range'
		write (ttyout,*) 'range = 0.01 to 1000.'
		go to 464
	}
	
	if (obits(imat) == 8) {
		bdscal(imat) = float(idn) / x
		qfscal(imat) = 255.0
	} else {

		bdscal(imat) = float(idn) / x
		qfscal(imat) = 32767
	}
	
			
	if (cmdverbose(-1) <= 1) {
		write (6,*)'band depth values will be multiplied by ',
				bdscal(imat)
		write (6,*)'quality of fit values will be ',
				'multiplied by ',qfscal(imat)
	}

	write (lunhist,463) obits(imat), idn, x, ihbcksl
463	format (i3, 3x, 'DN ',i6,' = ',f10.4, 9x, a,'# output bits, scale')

###########################################################
## compression of output if imaging

484	if (cmdverbose(-1) <= 1) write (ttyout,485)
485	format(///,'Enter: compress= none   or:',/,
		   '       compress= zip')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 484    # blank line, read again
	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	if (index(iopcon,'compress=') == 0) {
		call what (-1)
		write (ttyout,*) 'ERROR: compress= keyword not found'
		go to 484
	}
	i = index(iopcon,'compress=')+9
	call wjfren (i,x,il)
	if (iopcon(i-1:i+1) == 'zip' ) {
		ocompress(imat) = 1
		write (lunhist,486) 'compress= zip'
	} else if (iopcon(i-1:i+2) == 'none' ) {
		ocompress(imat) = 0
		write (lunhist,486) 'compress= none'
	} else {
		write (ttyout,*) 'ERROR: wrong compression keyword'
		call what(i)
		go to 484
	}

486	format (a)

###########################################################
## action

504	nxx = 0  # number of cases, default
	dosound(imat) = 0
	istartcase = 1
	do jj = 1, maxcse {          # default: do no cases
		iaction(imat,jj) = 0
	}

510	if (cmdverbose(-1) <= 1) write (ttyout,505)
505	format(///,'Enter: action: none   or:',/,
		   '       action: case #  (where # is a case number)',/,
		   '       action: sound1 "soundfile"',/,
		   '                 if no file name, use default output file',/,
		   '                 when done entering actions, enter:',/,
		   '       endaction')

	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i >= 80 & il == 0) go to 510    # blank line, read again

	#write (ttyout,*) 'DEBUG action: i=',i,' iopcon(i:80)=',iopcon(i:80)

	i = index(iopcon,'endaction')
	if (i > 0) {                 # end of action input
		#write (ttyout,*) 'DEBUG: endaction found'
		write (lunhist,*) 'endaction'
		go to 515
	}

	if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call what (-1)
		return
	}
	i = index(iopcon,'action:')
	if (i == 0) {
		call what (-1)
		write (ttyout,*) 'ERROR: action: keyword not found'
		go to 510
	}
	i = i + 7

	call wjfren(i,x,il)
	i = i -1
	
	if (i > 70) {
		call what(i)
		write (ttyout,*) 'ERROR: up to column 70: what action?'
		go to 510
	}

	#write (ttyout,*) 'DEBUG action: i=',i,' iopcon(i:80)=',iopcon(i:80)

	if (iopcon(i:i+3) == 'none') {

		#write (ttyout,*) 'DEBUG: action none found'

		write (lunhist,506) 'none'
506		format ('action: ',a)
		go to 510

	} else if (iopcon(i:i+5) == 'sound1') {  # get sound file
		i = i +6
		#write (ttyout,*) 'DEBUG: action sound1 found'
		do jj = i, 79 {
			if (iopcon(jj:jj) != ' ') break
		}
		if (jj > 78) {  # no file name, use base output file name

			sound1fil(imat) = mfile(imat)(1:lenfile(imat)) // '.wav'
			dosound(imat) = 1
			#write (ttyout,*) 'DEBUG: soundfile=',sound1fil(imat)
			write (lunhist,506) 'sound1'
			go to 510
		}
		isfirst = jj
		i = jj + 1
		if (jj > 78) {
			write (ttyout,*)'ERROR: no sound file name'
			go to 510
		}
		do jj = i, 80 {

#
#  KEL move comment before statement; RATFOR PC parsing problem
#			if (iopcon(jj:jj) == ' ') break # find next blank
#

#  find next blank
			if (iopcon(jj:jj) == ' ') break
		}
		islast = j - 1
		sound1fil(imat) = iopcon(isfirst:i)
		dosound(imat) = 1
		write (lunhist,506) 'sound1' // iopcon(isfirst:i)
		go to 510

	} else if (iopcon(i:i+3) == 'case') {

		#write (ttyout,*) 'DEBUG: action case found'

		i = i + 4
		jj1 = istartcase
		do jj = jj1, maxcse {
			#write (ttyout,*) 'DEBUG: case test ',jj,' i=',i
			call wjfren (i,x,il)
			if (il != 0) {
				call what (i)
				write (ttyout,509) il
509				format ('ERROR: invalid ',
					'character "',a1,
					'" when decoding case number')
				go to 510
			}
			if (jj > 1 & i > 78) go to 508
			ixx = x + 0.5
			#write (ttyout,*) 'DEBUG: x=',x,' i=',i,' ixx=',ixx
			if (ixx > 0 & ixx <= maxcse) {

				iaction(imat,jj) = ixx
				nxx = nxx + 1
				istartcase = istartcase + 1
			} else {
				#write (ttyout,*) 'DEBUG: i=',i
				call what(i)
				#write (ttyout,*) 'DEBUG: i=',i
				write (ttyout,*) 'ERROR: case ',ixx,
						'is out of range: 1 to ',
						 maxcse
				go to 510
			}

		}
508		write (lunhist,507) (iaction(imat,kk),kk = jj1, nxx)
507		format ('action: case',10(i4,1x))

		go to 510

	} else {
		write (ttyout,*) 'ERROR: that action not allowed'
		call what(i)
		go to 510
	}

515	continue

3112	write (lunhist,3111) ihbcksl
3111	format (a,'#',70('#'))

########} # end of do imat loop

go to 210

#DEBUG:
#	if (cmdverbose(-1) <= 1) {
#		write (ttyout,*) 'nmats=',nmats
#		write (ttyout,*) 'nfeats:',(nfeat(i), i= 1, nmats)
#	}

	end
