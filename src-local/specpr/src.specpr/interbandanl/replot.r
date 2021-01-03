subroutine replot(ifl1,xmin,xmax,lbnd,dataa,datsc1,chan1,chan2)



#ccc  name:
#ccc  version date:
#ccc  author(s): R. Clark and M. Klejwa
#ccc  language:
#ccc
#ccc  short description:
#ccc
#
# This routine will hopefully replot the screen 
#
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called:
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

#
	include "../common/alphabet"
	include "../common/label1"              # ititl,ihist
	include "../common/lbl3"		# error
	include "../common/lbl4"		# ubnd,(wmin,wmax)  
	include "../common/lbl6"		# idv1
	include "../common/lbl7"		# itrol
	include "../common/lblg"		# nchans,iline
	include "../common/hptrm"
	include "../common/lundefs"

	include "../common/overlys"
	include "../common/deletep"             # deleted point values
	include "../common/spcolors"


	real*4 xmax,xmin
	real*4 wvmin,wvmax
	real*4 dataa(4864),datsc1(4864)
	real*4 lbnd 
	integer*4 ifl1, chan1, chan2
	character*8 aname
	character*80 outline
	integer*4 ispcolor  # color
	integer*4 iov        #overlay number
	integer*4 ochan, iiov
	integer*4 il, i, j, itmpo, iero
	real*4    ovmin, ovmax, plotmin, plotmax
	real*4    ow1, ow2, oy1, oy2
	real*4 contin

# 
# Do all replotting stuff hopefully 
#
	                    call wavlng (itrol(1), itrol(2), ier)
#
#     write  identification to the plot
#
				call movabs(0,299*2)
                                call sb(0)
				call namdev (idv1, aname)
                                write(outline,40)ititl,idv1,ifl1,aname,char(0)
				call gwrite(outline)
				write(outline,41)ihist,char(0)
				call gwrite(outline)
#
#     write wavelength info
				call movabs(0,0)
				call sb(0)
				write(outline,42)itrol(1),itrol(2),nchans,char(0)
				call gwrite(outline)
#
#
#     draw the plot
#
				if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(0)   # black
				}
                                call alplty(lbnd,ubnd)
                                diff = ubnd-lbnd
                                if (itrol(3)==iha)
                                    call wvplta(nchans,wvmax,wvmin,iline)
                                if (itrol(3)==ihn)
                                    call iwplta(nchans,wvmax,wvmin,iline)
                                xmax = wvmax
                                xmin = wvmin
                                if (itrol(3)!=iha&&itrol(3)!=ihn)
                                    call chplta(nchans,xmax,xmin,dataa,iline)

			      # overlay block
			      for (iov=1; iov<=6; iov=iov+1) {
				if (ovrflg(iov) > 0 & ovrchn(iov) == nchans) {  # force autoscaling to 10% of range to match data
                                                                                # but only if channels match
				  ilino=2
				  #write(*,*) "DEBUG: crtplt overlay ", iov
				  #do jjxtmp= 1, ovrchn(iov) {
					#write (*,*) "DEBUG: overlay1=", jjxtmp, ovrdat1(jjxtmp), ovrwav1(jjxtmp)
				  #}
				  ovmin= 9.99e+33   # overlay min
				  ovmax=-9.99e+33   # overlay max
				  ochan=ovrchn(iov)
					# now remove continuum
			         if (chan1 > 0 & chan2 > chan1 & chan2 <= ochan) {
						for (iiov=1; iiov<=ochan; iiov=iiov+1) {  # delete allpoints in the scaled overlay
							ovrdsc(iiov,iov)= delpt
						}
						ow1=ovrwav(chan1,iov)
						ow2=ovrwav(chan2,iov)
						oy1=ovrdat(chan1,iov)
						oy2=ovrdat(chan2,iov)
						oslope=(oy2-oy1)/(ow2-ow1)   # rise/run
						offset=oy1-(oslope*ow1)
						#write(*,*) "DEBUG: removing overlay continuum, slope=",oslope," off=",offset
						for (iiov=chan1; iiov<=chan2; iiov=iiov+1) { # remove continuum
							if (ovrdat(iiov,iov) > delptup & ovrwav(iiov,iov) > delptup) {
								contin = oslope*ovrwav(iiov,iov)+offset
								ovrdsc(iiov,iov)=ovrdat(iiov,iov)/contin
								#write(*,*) "DEBUG: dat=",ovrdat(iiov,iov)," contin=",contin
							} else {
								ovrdsc(iiov,iov) = delpt   # deleted point
							}
						}	
						for (iiov=chan1; iiov<=chan2; iiov=iiov+1) {
							if (ovrwav(iiov,iov) >= xmin && ovrwav(iiov,iov) <= xmax) {
								if (ovrdsc(iiov,iov) < ovmin && ovrdsc(iiov,iov) > delptup) ovmin= ovrdsc(iiov,iov)
								if (ovrdsc(iiov,iov) > ovmax && ovrdsc(iiov,iov) > delptup) ovmax= ovrdsc(iiov,iov)
							}
						}
						plotmax=lbnd + diff - 0.10*diff
						plotmin=lbnd + 0.10*diff
						ovoff=(plotmin - ovmin*plotmax/ovmax)/(1.0 - ovmin/ovmax)
						ovscale=(plotmax -ovoff)/ovmax

						for (iiov=1; iiov<=ochan; iiov=iiov+1) {  # now scale the continuum removed overlay
							if (ovrdsc(iiov,iov) > delptup) {
								ovrdsc(iiov,iov)=  ovrdsc(iiov,iov) * ovscale + ovoff
							} else {
								ovrdsc(iiov,iov) = delpt   # deleted point
							}
						}
		                                if (igrmod >= 50 && igrmod <= 53) {
#XWIN							call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
						}
                               		   	call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdsc(1,iov),ovrwav(1,iov),ilino)
							#call crtin  # DEBUG
               		                   	if (igrmod >= 50 && igrmod <= 53) {
#XWIN							call xset_color(0)   # black
						}

				 } else {
					# end remove continuum
				  for (iiov=1; iiov<=ochan; iiov=iiov+1) { # find min and max in plot range
					if (ovrwav(iiov,iov) >= xmin && ovrwav(iiov,iov) <= xmax) {
						if (ovrdat(iiov,iov) < ovmin && ovrdat(iiov,iov) > delptup) ovmin= ovrdat(iiov,iov)
						if (ovrdat(iiov,iov) > ovmax && ovrdat(iiov,iov) > delptup) ovmax= ovrdat(iiov,iov)
					}
				  }
				  # find scale and offset
					plotmax=lbnd + diff - 0.10*diff
					plotmin=lbnd + 0.10*diff
					ovoff=(plotmin - ovmin*plotmax/ovmax)/(1.0 - ovmin/ovmax)
					ovscale=(plotmax -ovoff)/ovmax
					#write (*,*) "DEBUG: ovmax=",ovmax," ovmin=",ovmin," ovscale=",ovscale," ovoff=",ovoff

				  for (iiov=1; iiov<=ochan; iiov=iiov+1) {  # now scale the data
					if (ovrdat(iiov,iov) > delptup) {   # scale if not deleted point
						ovrdsc(iiov,iov)= ovrdat(iiov,iov) * ovscale + ovoff
					} else {
						ovrdsc(iiov,iov) = delpt   # deleted point
					}
				  }
                                  if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(iov+1)   # 2=red, 3=blue, 4=green, 5=orange, 6=magenta
				  }
                                  call crtplt(ovrchn(iov),xmax,xmin,lbnd,diff,ovrdsc(1,iov),ovrwav(1,iov),ilino)
					#call crtin  # DEBUG
                                  if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(0)   # black
				  }
				}
                               }
			      }
			      # end overlay block

				if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(spcolor)   # usr selected color
				}
                                call crtplt(nchans,xmax,xmin,lbnd,diff,datsc1,dataa,iline)
				if (igrmod >= 50 && igrmod <= 53) {
#XWIN					call xset_color(0)   # black
				}
                                call movabs(250*2,0)
                                call sb(0)
                                write(outline,50) char(0)
				call gwrite(outline)
                                call movabs(0,350*2)
                                call sb(0)
                            if (iauto==ihb)
                                call lprpct(1,nchans,lbnd,ubnd,dataa)
                            if (iauto==ihb)
                                return


40  format(1x,a,5x,'file= ',a,i5,' (',a,')',a1)
41  format(1x,'history=',a,a1)
42  format(1x,'wav: ',a1,i5,'  ch=',i5,a1)
50  format(1x,'h=channel,  a=wavelength,  n=energy',a1)
return
end
