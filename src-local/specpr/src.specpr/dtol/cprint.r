subroutine cprint(mode,mode2,ii,in,it,irec,iclin,irnum)
#	Ratfor
#********************** subroutine cprint ***********************


#ccc  name:
#ccc  version date:
#ccc  author(s):
#ccc  language:
#ccc
#ccc  short description:
#ccc
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

	implicit integer*4 (i-n)

	include "../common/label1"
	include "../common/lundefs"	

	integer*2 chkbit,ibit
	integer*4 yy,mm,dd
	character*11 xptime
	character*10 xpdate

	ibit=1
	itmp=chkbit(icflag,ibit)

	if (itmp==1) {                          # text mode
#   						(irnum=record number)

			write(ttyout,5) irnum, ititl, itxtch
			iclin=iclin+1

			if (it==1)  {
				call ptxtc(it,irec,iclin)
			}			

	}else {                                # data mode

#		call todms(iscta,24000,ihra,ihma,ss)
		call todms(isctb,24000,ihrb,ihmb,ssb)
		call frjuld(yy,mm,dd,jdateb)
		write(xptime,160) ihrb,ihmb,ssb
		write(xpdate,170) mm,dd,yy
		call zeroin(xptime)
		call zeroin(xpdate)

		if (in!=1) {

		if (mode2 != 1) {
			if (mode==0) {  # laboratory mode (default)

			     write(ttyout,10)irecno,ititl,itchan,xptime,xpdate
	
			}else {		# telescopic mode


		             airmas = float(irmas)/1000.0
			     write(ttyout,15)irecno,ititl,itchan,xpdate,airmas
			}

		}else{                  # write wavelenghts record #

			if (mode == 0) {
			
		       	    write (ttyout,25) irecno,ititl,itchan,xpdate,irwav

			}else{

			    write (ttyout,30) irecno,ititl,itchan,xpdate,irwav

	                }
		}

			if (ii==1){
				write(ttyout,17)ihist
			}
			


		}else{				#quick print
			write(ttyout,20) irecno
		}
	}

5	format(I6,2x,a,1x, i5,' Characters of TEXT')
10 	format(I6,2x,a,2x,I4,2x,a,2x,a)
15	format(I6,2x,a,2x,I4,2x,a,2x,f6.3)
17	format(10x,a)	
20	format(5x,I6)
25	format(I6,2x,a,2x,I4,2x,a,2x,I6)
30	format(I6,2x,a,2x,I4,2x,a,2x,I6)
160	format(I2,':',I2,':',F5.2)
170	format(I2,'/',I2,'/',I4)
	return
	end
