	subroutine wrtcrdrout (diaflg, xel, yel,
				lunresult, ttyout)

######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         wrtcrdrout
#ccc  version date: 12/12/1994
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: write out diagnostic output from cubecorder
#ccc
#ccc  algorithm description:
#ccc  system requirements: Unix
#ccc  subroutines called:
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
#ccc   diaflg: how much to write out
#ccc   xel, yel: pixel coordinates.
#ccc   ibest: which material.
#ccc    lunresult: lun of results file
#ccc    ttyout: lun for std out
#ccc---------------------------------------------------------------


	integer*4 diaflg, xel, yel, ibest
	integer*4 lunresult, ttyout
	integer*4 jj, ii, igroup

# arrays for multiple materials

	include "multmap.h"

	character*1 imch(5)

	# define feature importance characters

	imch(1) = 'O'
	imch(2) = 'W'
	imch(3) = 'D'
	imch(4) = '?'
	imch(5) = '?'


	if (diaflg == 0) {
		write (ttyout,*) ' '
		write (ttyout,*) ' '
		write (ttyout,108) yel,xel
108					format ('====',
			'Pxl(',I4,',',I4,') ',
			'====')
		write (ttyout,107) yel,xel
107			format ('CHOSEN OUTPUT:',/,
				'Pxl(',I4,',',I4,') ',
				'Grp/Cse Material  Fit  ',
				'  Depth    F*D')
		do igroup = 1, nzgroup {
			if (nmatgrp(igroup) == 0) next
			ibest = grpbest(igroup)
			write (ttyout,111) 'g',igroup,
				ibest,ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel),
				otitle(ibest)(1:24)
111			format (15x,a,i6,1x,i6,1x,f7.3,2x,
				f7.3,1x,f7.3,1x,a)
		}
	} else {
		write (ttyout,108) yel,xel
		write (ttyout,109)
109					format (/,' FITS, DEPTHS, F*D',
			' before best fit selection:',/)
		do jj = 1, nmats {
		   write (ttyout,112) (ii,jj,
			imch(featimprt(ii,jj)+1),
			zfit(ii,jj),
			zdepth(ii,jj),
			zfd(ii,jj),dln(ii,jj),
			zcompf(ii,jj),
			ii=1,nfeat(jj))
112					   format (' feat:',i3,
			'  mat:',i3,' type=',a,
			'  fit=',f6.3,
			'  depth=',f6.3,
			'  f*d=',f6.3,
			'  nrmlz=',f6.3,', ',
			   f4.1)
		}
		write (ttyout,*) ' '
		write (ttyout,107) yel,xel
		do igroup = 1, nzgroup {
			if (nmatgrp(igroup) == 0) next
			ibest = grpbest(igroup)
			write (ttyout,111) 'g',igroup,
				ibest,ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel),
				otitle(ibest)(1:28)
		}

	} 
	return
	end
