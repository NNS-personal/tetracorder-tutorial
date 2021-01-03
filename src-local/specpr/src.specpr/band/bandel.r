	subroutine bandel(ibnrm1,ibnrm2,nopts,delcnt,iopcon)
	implicit integer*4(i-n)
#cc  version date: 06/01/83
#cc  author(s): roger clark & jeff hoover
#cc  language:  fortran
#cc
#cc  short description:
#cc      this subroutine allows the user to delete the points.
#cc  algorithm description: none
#cc  system requirements:   none
#cc  subroutines called:
#cc                    crtin,wjfren
#cc  argument list description:
#cc      arguments: ibnrm1,ibnrm2,nopts,delcnt,iopcon
#cc  parameter description:
#cc  common description:
#cc  message files referenced:
#cc  internal variables:
#cc  file description:
#cc  user command lines:
#cc  update information:
#cc  notes:
#cc
	include "../common/lundefs"
	include "../common/blank"

	integer*2 delcnt
	character*80 outline
#
#  allows user to delete data points
#
write(outline,20) char(0)
call gwrite(outline)
write(outline,21) char(0)
call gwrite(outline)
repeat {
    call crtin
    i = 1
    k = 0
    iflag = 0
    repeat {
	call wjfren(i,t,it)
	if (t==0 && it==0) break 1
	if (it!=0) {
	    iflag = 1
	    write(outline,30) char(0)
            call gwrite(outline)
	} else {
	    ich = t
	    if (ich<ibnrm1 || ich>ibnrm2) {
		write(outline,40)ich, char(0)
		call gwrite(outline)
		iflag = 1
	    } else {
		k = k+1
		if (k>=nopts) go to 10
		if (datab(ich)==1) {
		    write(outline,60)ich, char(0)
		    call gwrite(outline)
		    iflag = 1
		    k = k-1
		}
		if (datab(ich)==0) datab(ich) = 1
	    }
	}
    }
    delcnt = delcnt+k
    dlcnt1 = delcnt+1
    if (dlcnt1>nopts) go to 10
    if (dlcnt1==nopts) break 1
    if (iflag!=1) return
}
write(outline,70) char(0)
call gwrite(outline)
delcnt = 4864
call crtin
return

10  write(outline,50)delcnt, char(0)
    call gwrite(outline)
    write(outline,51) char(0)
    call gwrite(outline)
    delcnt = 4864
    call crtin
    return

20  format(' enter channel no(s).',a1)
21  format(' aaaa bbbb cccc dddd',a1)
30  format(' channel no. contains alphabetic',a1)
40  format(' channel ',i5,' is not within band',a1)
50  format(' too many points (',i5,') deleted',a1)
51  format(' start all over',a1)
60  format(' ch ',i5,' already deleted',a1)
70  format(' move band limits to process one point',a1)

end
