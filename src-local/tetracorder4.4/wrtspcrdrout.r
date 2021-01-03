	subroutine wrtspcrdrout (diaflg, inamr, ifils, ititl,
				xel, lunresult, ttyout)

#######	implicit integer*4 (i-n)
	implicit none

#ccc  name:         wrtspcrdrout
#ccc  version date: 
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: write out diagnostic results from specorder.
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
#ccc    diagflg: how much to poutput
#ccc    inamr: name of specpr file where spectrum came from
#ccc    ifils: file id of specpr file where spectrum came from
#ccc    ititl: title of spectrum
#ccc    lunresult: lun of results file
#ccc    ttyout: lun for std out
#ccc---------------------------------------------------------------

# arrays for multiple materials

	include "multmap.h"

	integer*4 diaflg, ifils, ibest, lunresult, ttyout
	integer*4 xel, jj, ii, intmp, igroup
	integer*4 ntmpnotmat, ntmpnotfeat
	character*8 inamr
	character*40 ititl
	character*512 soundstring

	character*1 imch(5)

	# define feature importance characters

	imch(1) = 'O'
	imch(2) = 'W'
	imch(3) = 'D'
	imch(4) = '?'
	imch(5) = '?'


# Write diagnostic output at user specified intervals

	if (diaflg == 0) {
		call eralph
		do igroup = 1, nzgroup {
			if (nmatgrp(igroup) == 0) next
			ibest = grpbest(igroup)
			if (ofit(ibest,xel) < 0.1e-4) {
				write (ttyout,107) inamr, ifils, ititl,igroup
				write (lunresult,107) inamr, ifils, ititl,igroup
107				format ('Spectrum: ',a,i7,3x,a,
					'HAS NO MATCH to group ',i3,
					' spectral library set')
			} else {
				write (ttyout,108) inamr, ifils, ititl,
					mtitle(ibest),
					ofit(ibest,xel),
					odepth(ibest,xel),
					ofd(ibest,xel)
108				format ('Spectrum: ',a,i7,2x,a,
					'MATCHES ',a,
					' Fit=',f7.4,' D=',f8.4,
					' FD=',f8.4)
				write (lunresult,108) inamr, ifils, ititl,
					mtitle(ibest),
					ofit(ibest,xel),
					odepth(ibest,xel),
					ofd(ibest,xel)
			  	if (dosound(ibest) == 1) {    # output answer as sound

					soundstring= 'tricordersound ' // sound1fil(ibest) // ' ' // char(0)
					#write (ttyout,*) 'DEBUG: ', soundstring

					call system (soundstring)
			  	}
			}
		}
	} else if (diaflg == 1) {
		call eralph
		do igroup = 1, nzgroup {
			if (nmatgrp(igroup) == 0) next
			ibest = grpbest(igroup)
			if (ofit(ibest,xel) < 0.1e-4) {
			  write (ttyout,107) inamr, ifils, ititl,igroup
			  write (lunresult,107) inamr, ifils, ititl,igroup
			} else {
			  write (ttyout,111) inamr, ifils, ititl,
				mtitle(ibest),
				ibest,
				ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel)
111			  format ('The spectrum: ',a,i7,3x,a,//,
				'is best matched by',14x,a,/,
				32x, 40('='),//,
				' Material  Fit  ',
				'    Depth        F*D',/,
				(i6,1x,f7.4,3x,
					f8.4,3x,f7.4,/))
			  write (lunresult,111) inamr, ifils, ititl,
				mtitle(ibest),
				ibest,
				ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel)

			  if (dosound(ibest) == 1) {     # output answer as sound

				soundstring= 'tricordersound ' // sound1fil(ibest) // ' ' // char(0)
				#write (ttyout,*) 'DEBUG: ', soundstring

				call system (soundstring)
			  }
			}
		}
	} else {
		call eralph
		if (diaflg == 3) {
		  write (ttyout,109)
		  write (lunresult,109)
109					  format (/,17x,' FITS, DEPTHS, F*D',
			' before best fit selection:',/,
			10x,'Title',19x,
			'Mat Feat',
			1x,'T    Fit   Depth',
			'    f*d      Nrmlz')
		  do jj = 1, nmats {
			if (group(jj) > -1) { # valid group
			    write (ttyout,112) (mtitle(jj)(1:32),
				jj,ii,
				imch(featimprt(ii,jj)+1),
				zfit(ii,jj),
				zdepth(ii,jj),
				zfd(ii,jj),dln(ii,jj),
				zcompf(ii,jj),
				ii=1,nfeat(jj))
			    write (lunresult,112) (mtitle(jj)(1:32),
				jj,ii,
				imch(featimprt(ii,jj)+1),
				zfit(ii,jj),
				zdepth(ii,jj),
				zfd(ii,jj),dln(ii,jj),
				zcompf(ii,jj),
				ii=1,nfeat(jj))
112			    format (a,1x,i4,1x,
				i4,1x,a,1x,f6.3,
				1x,f7.4,1x,f7.4,1x,
				f6.3,',',f4.1)
			#   write (ttyout,*) 'DEBUG: numnotfeat=',numnotfeat(jj),
			#			' ofit=',ofit(jj,xel)
			#   write (lunresult,*) 'DEBUG: numnotfeat=',numnotfeat(jj),
			#			' ofit=',ofit(jj,xel)
			    if (numnotfeat(jj) > 0) { # have NOT features
				do intmp = 1, numnotfeat(jj) {
					ntmpnotmat = notmat(intmp,jj)
					ntmpnotfeat= notfeat(intmp,jj)
					# NOT feature is possible
					if (notflg(intmp,jj) > 0) {
								# NOT is found

					   write (ttyout,*) ' NOT CONDITION: ',
						'feat',ntmpnotfeat,
						', material',ntmpnotmat,
						' excludes material ',jj

					   write (lunresult,*) ' NOT CONDITION: ',
						'feat',ntmpnotfeat,
						', material',ntmpnotmat,
						' excludes material ',jj
					} else {

					   write (ttyout,*) ' NO NOT condition:',
						'on material', jj,
						' feat',ntmpnotfeat,
						', material',ntmpnotmat
					   write (lunresult,*) ' NO NOT condition:',
						'on material', jj,
						' feat',ntmpnotfeat,
						', material',ntmpnotmat
					}
				}
			     }
			}
		  }
		  write (ttyout,*) ' '
		  write (lunresult,*) ' '
		}

		write (ttyout,114)
		write (lunresult,114)
114		format ('Weighted Fits, Depths,',
			' and F*Ds values:',/,
			10x,'Title',25x,' Group Material',
			2x,'Fit',4x,'Depth',4x,'F*D')
		do jj = 1, nmats {
			if (group(jj) > -1) { # valid group
			   write (ttyout,113) mtitle(jj),
				group(jj),jj,
				ofit(jj,xel),
				odepth(jj,xel),
				ofd(jj,xel)
			   write (lunresult,113)  mtitle(jj),
				group(jj),jj,
				ofit(jj,xel),
				odepth(jj,xel),
				ofd(jj,xel)
113			   format (a,1x,i5,1x,i5,2x,f6.3,1x,f7.4,1x,f7.4)
			}
		}
		write (ttyout,*) ' '
		write (ttyout,*) ' '
		write (lunresult,*) ' '
		write (lunresult,*) ' '

		do igroup = 1, nzgroup {
			#write (ttyout,*) 'DEBUG: group:',igroup,nmatgrp(igroup),nzgroup,grpbest(igroup)
			if (nmatgrp(igroup) == 0) next
			ibest = grpbest(igroup)
			if (ofit(ibest,xel) < 0.1e-4) {
			  write (ttyout,107) inamr, ifils, ititl,igroup
			  write (lunresult,107) inamr, ifils, ititl,igroup
			} else {
			  write (ttyout,111) inamr, ifils, ititl,
				mtitle(ibest),
				ibest,
				ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel)
			  write (lunresult,111) inamr, ifils, ititl,
				mtitle(ibest),
				ibest,
				ofit(ibest,xel),
				odepth(ibest,xel),
				ofd(ibest,xel)
			  if (dosound(ibest) == 1) {     # output answer as sound

				soundstring= 'tricordersound ' // sound1fil(ibest) // ' ' // char(0)
				#write (ttyout,*) 'DEBUG: ', soundstring

				call system (soundstring)
			  }
			}
		}

	} 

###DEBUG:
	do igroup = 1, nzgroup {
		#write (ttyout,*) 'DEBUG: group: ',igroup,' best mat= ',grpbest(igroup)
	}

	return
	end
