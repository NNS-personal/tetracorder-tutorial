C Output from Public domain Ratfor, version 1.0
      subroutine crtpsc (igo,bbnd,ubnd,nchans,datac,error,wmina,wmaxa,i,
     *dataa,iscale)
      implicit integer*4 (i-n)
Cccc  version date: 06/01/83
Cccc  author(s): Roger Clark & Jeff Hoover
Cccc  language:  Ratfor
Cccc
Cccc  short description:
Cccc                   This subroutine allows the user to set the
Cccc                   horizontal and vertical scales for the crt
Cccc                   plotting routine.
Cccc  algorithm description: none
Cccc  system requirements:   none
Cccc  subroutines called:
Cccc                    crtin,rlchng,wjfren,er,eralph
Cccc  argument list description:
Cccc     arguments: igo,bbnd,ubnd,nchans,datac,error,wmina,wmaxa,i
Cccc  parameter description:
Cccc  common description:
Cccc  message files referenced:
Cccc  internal variables:
Cccc  file description:
Cccc  user command lines:
Cccc  update information:
Cccc  NOTES:
Cccc
C################################################################
C                                                               #
C       this routine allows the user to set the horizontal      #
C       and vertical scale for the crt plotting routine.        #
C                                                               #
C       Author: Roger N. Clark                                  #
C       Modified:       JAH 03-01-83                            #
C                                                               #
C################################################################
      common /hptrm/ igrmod,ihpout,imode,ixlast,iylast,ipen
      common /hptrm/ iot
      character*80 ihpout
      integer*4 igrmod,imode,ixlast,iylast,ipen,iot
      integer*4 luntxt,ulun,ylun,ttyin,ttyout,dlun,vlun,wlun
      integer*4 addlun,wavlun,lstlun,pltlun,wvhlun
      integer*4 ttllun,cmdlun,slun,rlun,cpylun,wrtlun,redlun
      parameter (luntxt=1)
      parameter (ulun=3)
      parameter (ylun=4)
      parameter (ttyin=5)
      parameter (ttyout=6)
      parameter (dlun=7)
      parameter (vlun=8)
      parameter (wlun=9)
      parameter (addlun=10)
      parameter (redlun=10)
      parameter (wrtlun=11)
      parameter (wavlun=11)
      parameter (lstlun=12)
      parameter (pltlun=13)
      parameter (wvhlun=14)
      parameter (ttllun=15)
      parameter (cmdlun=16)
      parameter (slun=17)
      parameter (rlun=18)
      parameter (cpylun=19)
      common /alpha/ iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj, ihk,ihl,ih
     *m,ihn,iho,ihp,ihq,ihr,ihs,iht, ihu,ihv,ihw,ihx,ihy,ihz
      common /alpha/ ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,ihci,ihcj, 
     *ihck,ihcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct, ihcu,ihcv,ihcw,
     *ihcx,ihcy,ihcz
      common /digit/ ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9
      common /asciic/ihprd,ihandp
      integer*4 iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj, ihk,ihl,ihm,ihn
     *,iho,ihp,ihq,ihr,ihs,iht, ihu,ihv,ihw,ihx,ihy,ihz
      integer*4 ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,ihci,ihcj, ihck,
     *ihcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct, ihcu,ihcv,ihcw,ihcx,
     *ihcy,ihcz
      integer*4 ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9
      integer*4 ihprd,ihandp
C     /lbl7/ common:
C       the following are device control variables:
C       idevc (1) : file w
C       idevc (2) : file v
C       idevc (3) : file d
C       idevc (4) : file s
C
C       idevc  = 0:  dummy
C       idevc  =-1:  disk 1
C       idevc  =-2:  disk 0
C       idevc  =-3:  mag tape 0
C       idevc  =-4:  mag tape 1
C
C       idvc  : used to indicate v file and w file device:
C         idvc (1,j) = mag tape, idvc (2,j) = disk.
C       idevc and idvc were originally used in the mit version of the program
C         but need to be phased out ( they're partially phased out now )
C         completely.  use the volume numbers for this control now.  (in
C             /lblvol/ common).
C       itrol (1) : wavelength file id: V W D U or Y in use
C       itrol (2) : wavelength record currently in use.
C       itrol (3) : channel, wavelength, energy plot control flag.
C       itl  : title option l selected by the user in math operations.
C
      common /lbl7/ idevc(4), idvc(2,4), itl, itrol(3)
      character*40 itl
      integer*4 idevc, idvc, itrol
      real*4 datac(4864),error(4864),dataa(4864)
      character*1 iichar
C = a autoscale, A= autoscale +2% range, B= autoscale, keep 0
      integer*4 iscale
C
C inverse wavelength mode
      inflag=0
C bbnd = 0 autoscale mode izflag=1
      izflag=0
C izflag = 2 scale 2% around min, max
C
C     this subroutine determines the crt plot scale (horizontal and
C     vertical axes).
C
      if(igo .eq. 6)then
      call eralph
      endif
      if(igo .eq. 6)then
      go to 6
      endif
      if(igo .eq. 8)then
      go to 8
      endif
      if(iscale .eq. ihcb)then
C autoscale with bbnd = 0 
      izflag = 1
      endif
      if(iscale .eq. ihca)then
C autoscale 2% around min, max
      izflag = 2
C
C     auto scale if desired
C
      endif
9     bbnd= 0.9e+30
      ubnd= -0.9e+30
      jb=nchans
      ja=1
C count of number of valid channels in the range
      icount=0
      xmax = wmaxa
      xmin = wmina
      if(xmax .lt. xmin)then
      xmax = wmina
      xmin = wmaxa
Cwrite (ttyout,*) 'DEBUG: xmax < xmin',xmax, xmin
      endif
      if(itrol(3).eq.ihn)then
Cwrite (ttyout,*) 'DEBUG: converting xmin, xmax to wavenumber'
      if(abs(xmax) .lt. 0.1e-25)then
      xmax = 0.1e-25
      endif
      if(abs(xmin) .lt. 0.1e-25)then
      xmin = 0.1e-25
      endif
      xtmp = xmin
      xmin = 10000.0/xmax
      xmax = 10000.0/xtmp
Cwrite (ttyout,*) 'DEBUG: xmin= ',xmin,'  xmax=', xmax, '  x(3654)=',dataa(3654)
      endif
      do23018 jmn= ja, jb 
      if(datac(jmn) .eq. -1.23e34)then
      goto 23018
      endif
      if(dataa(jmn).gt.xmax .or. dataa(jmn).lt.xmin)then
      goto 23018
      endif
      xx= datac(jmn)+ error(jmn)
      yy= datac(jmn)- error(jmn)
      if(yy .lt. bbnd)then
      bbnd= yy
      endif
      if(xx .gt. ubnd)then
      ubnd= xx
      endif
      icount = icount+1
23018 continue
23019 continue
      if(icount .lt. 1)then
C no valid channels found, so set range 0 to 2
      bbnd = 0.0
      ubnd = 2.0
      endif
      if(izflag .eq. 1)then
      bbnd = 0.0
      endif
      if(izflag .eq. 2)then
C autoscale range is 2% more than data range
      diff= abs(ubnd-bbnd)
      ubnd = ubnd + 0.02*diff
      bbnd = bbnd - 0.02*diff
C
C     if difference between upper and lower bound is too small, default
C     to 0.0 to 2.0
C
      endif
      diff= ubnd-bbnd
      if(diff .lt. 0.0)then
      go to 6
      endif
      if(ubnd .gt. 0.0)then
      ubnd= ubnd*1.02
      else
      ubnd= ubnd*0.98
      endif
      if(bbnd .gt. 0.0)then
      bbnd= bbnd*0.98
      else
      bbnd= bbnd*1.02
      endif
      call er
8     if(i .ne. ihc)then
      go to 7
C
      endif
6     if(wmaxa .le. wmina)then
      write (ttyout,62) bbnd,ubnd
      else
      write (ttyout,61) bbnd,ubnd,wmina,wmaxa
      endif
      write (ttyout, 3)
      call crtin
      j= 1
C
C     decode lower and upper bound
C
      call rlchng(j, xx, il)
      inflag=0
      izflag=0
      if(j.ge.80)then
      go to 4
C	if (il == ihn) go to 4
      endif
      if(il .eq. ihw)then
      go to 19
      endif
      if(il .eq. ihn)then
      inflag=1
      go to 19
      endif
      if(il .eq. ihca)then
      call getiopconchar(j,iichar)
      if(iichar.eq.'0')then
      izflag=1
      endif
      if(iichar.eq.'z')then
      izflag=1
      endif
      if(iichar.eq.'2')then
      izflag=2
C		write (*,*) 'DEBUG: autoscale zero flag= ',iichar
      endif
      i= 0
      call hreset(1)
      go to 9
      endif
      if(il .ne. 0)then
C error encountered
      call what(j)
      go to 16
      endif
      bbnd=xx
      call rlchng(j, xx, il)
      if(j.ge.80)then
      go to 4
      endif
      if(il .ne. 0)then
C error encountered
      call what(j)
      go to 16
      endif
      ubnd=xx
C
      call er
      if(igrmod .ge. 50 .and. igrmod .le. 53)then
C X-windows case only
      call eralph
      endif
      igo=0
      return
C
19    call rlchng(j,xx,il)
      if(il .ne. 0)then
C error encountered
      call what(j)
      go to 16
      endif
      call rlchng(j,yy,il)
      if(il .ne. 0)then
C error encountered
      call what(j)
      go to 16
      endif
      if(yy .lt. xx)then
      go to 16
      endif
      if((yy .eq. xx) .and. (xx .ne. 0.))then
      go to 16
      endif
      if(inflag.eq.1)then
      if(xx.ne.0)then
      xx=10000.0/xx
      else
      xx=0
      endif
      if(yy.ne.0)then
      yy=10000.0/yy
      else
      yy=0
      endif
      if(xx.gt.yy)then
      xtmp=xx
      xx=yy
      yy=xtmp
      endif
      endif
      wmina=xx
      wmaxa=yy
      go to 6
4     igo=4
      call er
      if(igrmod .ge. 50 .and. igrmod .le. 53)then
C X-windows case only
      call eralph
      endif
      return
7     igo=7
      if(igrmod .ge. 50 .and. igrmod .le. 53)then
C X-windows case only
      call eralph
      endif
      return
16    write (ttyout,17)
      go to 6
17    format (' ** ERROR: reenter')
62    format (' Current Scale: VERTICAL=', 1pe11.4,'  ', 1pe11.4, /, '  
     *              HORIZ.  = Automatic')
C RED Added comma before first 1pe11.4 in second line
61    format (' Current Scale: VERTICAL=',1pe11.4, '  ', 1pe11.4, /, '  
     *              HORIZ.  =',1pe11.4, '  ', 1pe11.4)
3     format( /,' To scale the plot, type in the mode (n or w) and ', 'h
     *orizontal axis limits first.',/,' When the vertical scale is enter
     *ed, ', 'the routine will exit to the plot.',//,' HORIZONTAL:',/,' 
     *type  n  and left and right hand limits in ', 'INVERSE WAVELENGTH,
     * or:',/,' type  w  and left and right hand WAVELENGTH limits', /,'
     *          (if you type  w  only,  the program will ', 'AUTOSCALE t
     *he limits ', /,'          from the current wavelength set)', //,' 
     *VERTICAL:',/,' Type lower bound and upper bound values for ', 'the
     * VERTICAL AXIS, or:',/,' type  A  to AUTO SCALE (the VERTICAL AXIS
     *), or:',/)
      end
