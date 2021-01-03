	subroutine tri1setup

	implicit integer*4(i-n)

#ccc  name:         tri1setup
#ccc  version date:
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: setup stuff for tetracorder
#ccc                     program start info, history file
#ccc                     and wavelength set.
#ccc
#ccc  algorithm description:
#ccc  system requirements: Unix
#ccc  subroutines called: many specpr routines, need specpr.a library
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:


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

# basic tetracorder parameters

        include "multmap.h"

	include "tri1.h"

	integer*4 iwtmpf
#RED just to be consisent
	integer iwidok  # function

# local variables
	real*4 a, b, x
	integer*4 icrst, ik, il, itmp, jend, length, lnb
	integer*4 ier, i, ic
	integer*4 ihour, imm, mm, dd, yy, jda
	real*4 ss
	integer*4 cmdverbose   # function cmdverbose

	logical*4       fexist     # file exists: true, false if doesn't

	ihbcksl = char(92)  # this is the backslash character

	call fuser(uname)  # get the user name

# Print program information to user

	write (ttyout,115) iversion
115     format (20x,
'===== Program Tetracorder (version',f4.1,') =====',//,
'     Roger N. Clark, U.S. Geological Survey, Denver, Colorado',/,
'     rclark@speclab.cr.usgs.gov',//,
'     This program maps multiple materials using multiple spectral', /,
'     spectral features per material in an imaging spectrometer' /,
'     data set.',/,
'     or with individual spectra',//,
'     The image cube data sets MUST be BIL Integer*2 format.',//,
'     The image cube output is 3 byte images per material:',/,
'              weighted fit',/,
'              weighted band depth',/,
'              weighted fit times band depth.',/,
'     The input image cube must be assigned as a specpr file',//,
'     Analysis results from individual spectra ',
	'are located in the results file',/)

	write (ttyout,114) maxmat, maxfeat, imaxch, maxpix, maxnotfeat
114	format ('     The current limits on mapping are:',/,
		11x,i5, ' materials,',13x,i5,
			' features/material,',/,
		11x,i5, ' channels/spectrum,',5x,i5,
			' pixels/scan line.',/,
		11x,i5, ' "NOT" features/material',/)

44	if (cmdverbose(-1) <= 1) write (ttyout,45)
45	format (' Enter the name of the history file')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i > 1) i = i-1
	jend=lnb(iopcon)
	length = jend - i + 1
	lengthhist = length
	if (length > 80) length = 80
	jend = i + length - 1
	histfile(1:length) = iopcon(i:jend)      # check for blanks
	itmp = index(histfile(1:length),' ')
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,184)
184		format (' ERROR: blanks are not allowed in file names',/)
		go to 44
	}
	itmp = index(histfile(1:length),char(9))  # check for tabs
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,185)
185		format (' ERROR: tabs are not allowed in file names',/)
		go to 44
	}

	inquire (file=histfile(1:length),exist=fexist)

	if (fexist) {

		write (ttyout,183) histfile(1:length)
183		format ('Note: history file already exists, appending to',
			/,a)

		open (unit=lunhist, file=histfile(1:length),
			access='sequential', form='formatted',
			status='old', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'history', histfile
187			format (' OPEN ERROR',i5,' on ',a,' file:',/,a)
			go to 44
		}
		call flushseqfile(lunhist,histfile(1:length),ier)

		if (ier != 0) go to 44

		write (lunhist,190) ihbcksl,ihbcksl,ihbcksl,
					ihbcksl,ihbcksl

	} else {

		open (unit=lunhist, file=histfile(1:length),
			access='sequential', form='formatted',
			status='new', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'history', histfile
			go to 44
		}
	}

# write beginning history
	write (lunhist,3000) ihbcksl, iversion, histfile(1:length)
3000	format (a,'# USGS Tetracorder, version',f4.1,'  history',/, a)

# Enter results file

144	if (cmdverbose(-1) <= 1) write (ttyout,145)
145	format (' Enter the name of the results file')
	call crtin
	i = 1
	call wjfren (i,x,il)
	if (i > 1) i = i-1
	jend=lnb(iopcon)
	length = jend - i + 1
	if (length > 80) length = 80
	jend = i + length - 1
	resultfile(1:length) = iopcon(i:jend)      # check for blanks
	itmp = index(resultfile(1:length),' ')
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,184)
		go to 144
	}
	itmp = index(resultfile(1:length),char(9))  # check for tabs
	if (itmp != 0) {
		call what(i+itmp)
		write (ttyout,185)
		go to 144
	}

	inquire (file=resultfile(1:length),exist=fexist)

	if (fexist) {

		write (ttyout,189) resultfile(1:length)
189		format ('Note: results file already exists, appending to',
			/,a)

		open (unit=lunresult, file=resultfile(1:length),
			access='sequential', form='formatted',
			status='old', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'results file', resultfile
			go to 144
		}

		call flushseqfile(lunresult,resultfile(1:length),ier)

		if (ier != 0) go to 144

		write (lunresult,190) ihbcksl,ihbcksl,ihbcksl,
					ihbcksl,ihbcksl
190		format (a1,'#',/,a1,'#',78('='),/,
			a1,'# ',11(' NEWRUN'),/,a1,'#',78('='),/,a1,'#')

	} else {
		open (unit=lunresult, file=resultfile(1:length),
			access='sequential', form='formatted',
			status='new', iostat=ier)

		if (ier != 0) {
			write (ttyout,187) ier, 'results file', resultfile
			go to 144
		}
	}

# write beginning results
	call jdatim(jda,isec)
	call todms (isec,1,ihour,imm,ss)
	call frjuld (yy,mm,dd, jda)
	write (lunresult,3003) ihbcksl, iversion, ihbcksl,
				mm,dd,yy,ihour,imm,ss,
				uname, ihbcksl, ihbcksl,
				resultfile(1:length),
				ihbcksl, ihbcksl,
				histfile(1:lengthhist)
3003	format (a,'# USGS Tetracorder, version',f4.1,/,
		a1,'# Analysis started on ',
		i2,'/',i2,'/',i4,' (mm/dd/yyy) ',
		i2,':',i2,':',f3.0,' UT   by user:',a,/,
		a1,'# results in file:',/, a1,'# ',a,/,
		a1,'# history in file:',/, a1,'# ',a)

	write (lunhist,3004)   ihbcksl,
				mm,dd,yy,ihour,imm,ss,
				uname, ihbcksl,
				resultfile(1:length)

3004	format (a1,'# Analysis started on ',
		i2,'/',i2,'/',i4,' (mm/dd/yyy) ',
		i2,':',i2,':',f3.0,' UT   by user:',a,/,
		a1,'# results in file:',/,a)


50	if (cmdverbose(-1) <= 1) write (ttyout,116)
116	format (1x,
'    Enter wavelength file id and record number, (e.g. V23),',//,
'     or press return, or "e", or "x" to exit:',/)

140	call crtin
	i=1
150	call wjfren (i,a,il)
	if (iwidok(il) == 1) {
		iwtmpf = il
		call wjfren (i,b,ik)
		if (b<=0 || b>maxrec) {
			call what (i)
			write(ttyout,165)
165     		format (' *** ERROR -- ',
				'invalid input re-enter ***:',/)

			go to 50
		} else {
			irecw=b
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 50
			}
			itrol(1) = iwtmpf
			itrol(2) = irecw
			call whedr2
		}
		if (ik!=0) i=i-1
		go to 150
	} else  if (il==ihx || il==ihe)  {
		ic=il
		icrst = 1
		call rstart(icrst)
		call closef
		stop
	} else {
			irecw=itrol(2)
			iwtmpf = itrol(1)
			call wavlng (iwtmpf,irecw,ier)
			if (ier != 0) {
				write (ttyout,165)
				go to 50
			}
	}


	call namdwv(itrol(1),inamwi)  #name of input wavelength set
	if (nchans > imaxch) {
		call what (-1)
		write (ttyout,*) 'ERROR: number of channels:',nchans	
		write (ttyout,*) '     is larger than limit:',imaxch	
		go to 50
	}

# write history
	write (lunhist,3010) iwtmpf,irecw,ihbcksl
3010	format (a1,i7,14x,a,'# Wavelength set to use')

	return
	end

