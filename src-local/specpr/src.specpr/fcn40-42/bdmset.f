C Output from Public domain Ratfor, version 1.0
      subroutine bdmset (wav,rflib,cl1,cl2,cr1,cr2, rflibc,minch,maxch,i
     *feattype,ier)
      implicit integer*4 (i-n)
Cccc  name:  bdmset
Cccc  version date:  January 29, 1990
Cccc  author(s):  Roger N. Clark
Cccc  language:  Ratfor
Cccc
Cccc  short description: 
Cccc		This program computes non-changing library reference data
Cccc            for the bandmp subroutine.
Cccc
Cccc  algorithm description:
Cccc                    given continuum removed library spectrum,
Cccc                    add a constant to the spectrum and renormalize
Cccc                    to modify band depth such that there is a least
Cccc                    squares fit to the observed spectrum.
Cccc
Cccc                    THIS subroutine computes the non-changing
Cccc                         components of the least squares solution.
Cccc  system requirements: none
Cccc  subroutines called: none
Cccc
Cccc
Cccc  parameter description:
Cccc     INPUT:
Cccc        rflib  = reference library spectrum  R*4 (cr2 elements)
Cccc        cl1    = continuum point begin on left side of band  I*4
Cccc        cl2    = continuum point end on left side of band  I*4
Cccc                   note: cl2 >= cl1  (checked)
Cccc        cr1    = continuum point begin on right side of band  I*4
Cccc                   note: cr1 > cl2 + 1 (checked)
Cccc        cr2    = continuum point end on right side of band  I*4
Cccc                   note: cr2 >= cr1 (checked)
Cccc                   note: cr2 also determines the max array sizes
Cccc     OUTPUT:
Cccc        rflibc = reference library spectrum, continuum
Cccc                                 removed  R*4 (cr2 elements)
Cccc        minch  = minimum channel in the reference library spectrum.
Cccc                 This is defined to be the band minimum.
Cccc        maxch  = maximum channel in the reference library spectrum.
Cccc                 This is defined to be the band maximum.
Cccc        ifeattype = -1 is an emission feature
Cccc                  =  1 is an absorption band
Cccc        ier    = error detected in input:
Cccc                    = 0 no error
Cccc                    = 1 error
Cccc
Cccc  common description: none
Cccc  message files referenced: none
Cccc  internal variables:
Cccc  file description: none
Cccc  user command lines: none
Cccc  update information:
Cccc  NOTES:
Cccc
      integer*4 ttyout
      integer*4 cl1,cl2,cr1,cr2,ier,minch,maxch,ifeattype
      real*4 wav(cr2), rflib(cr2)
      real*4 rflibc(cr2)
      ttyout = 6
      minch=0
      maxch=0
C
C check input parameters are correct
C
      ier = 0
      if(cl1 .gt. cl2)then
      write (ttyout, 100)
100   format (' ERROR: left continuum segment point 1 is', ' greater tha
     *n point 2')
      ier = 1
      endif
      if(cr1 .gt. cr2)then
      write (ttyout, 101)
101   format (' ERROR: right continuum segment point 1 is', ' greater th
     *an point 2')
      ier = 1
      endif
      if(cl2 + 1 .gt. cr1 - 1)then
      write (ttyout, 102)
102   format (' ERROR: left continuum segment point 2 + 1 ', 'channel is
     * greater than ',/, '        right continuum segment point 1 - 1 ch
     *annel',/, '        This means there are no channels for', ' the ac
     *tual absorption band' )
      ier = 1
      endif
      if(ier .eq. 1)then
      return
C
C deleted point value:
C
      endif
      delpt = -1.23e34
C
C compute continuum correction to ref spectrum
C
C       avlr = average left  side of continuum
C       avrc = average right side of continuum
C       avwlc = average wavelength left side continuum
C       avwrc = average wavelength right side continuum
C first left continuum average:
      avlc = 0.0
      avwlc = 0.0
      n = 0
      do23008 i = cl1, cl2 
      if(wav(i) .eq. delpt .or. rflib(i) .eq. delpt)then
      goto 23008
      endif
      avlc = avlc + rflib(i)
      avwlc = avwlc + wav(i)
      n = n +1
23008 continue
23009 continue
      if(n .lt. 1)then
      write (ttyout,120)
120   format (' ERROR: all points in reference spectrum,', ' left contin
     *uum deleted.')
      ier = 1
      return
      endif
      avlc = avlc / float(n)
      avwlc = avwlc / float(n)
C now right continuum average:
      avrc = 0.0
      avwrc = 0.0
      n = 0
      do23014 i = cr1, cr2 
      if(wav(i) .eq. delpt .or. rflib(i) .eq. delpt)then
      goto 23014
      endif
      avrc = avrc + rflib(i)
      avwrc = avwrc + wav(i)
      n = n +1
23014 continue
23015 continue
      if(n .lt. 1)then
      write (ttyout,121)
121   format (' ERROR: all points in reference spectrum,', ' right conti
     *nuum deleted.')
      ier = 1
      return
      endif
      avrc = avrc / float(n)
      avwrc = avwrc / float(n)
C now we have two pairs of x,y points to derive the continuum line
C       thus: wav = a * rflib  + b
      bottom = avwrc - avwlc
      if(abs(bottom) .lt. 0.1e-20)then
      write (ttyout, 130)
130   format (' ERROR: wavelength range of continuum is',' too small!')
      ier = 1
      return
      endif
      a = (avrc - avlc)/bottom
      b = avrc - a * avwrc
C
C now we can continuum correct the reference spectrum.
C     compute rflibc
C
      if(cl1 .gt. 1)then
      do23024 i = 1, cl1-1 
      rflibc(i) = delpt
23024 continue
23025 continue
      endif
      do23026 i = cl1, cr2 
      if(wav(i) .eq. delpt .or. rflib(i) .eq. delpt)then
      rflibc(i) = delpt
      else
      contin = a * wav(i) + b
      if(abs(contin) .gt. 0.1e-20)then
      rflibc(i) = rflib(i) / contin
      else
      rflibc(i) = delpt
      endif
      endif
C
C now find the band minimum and maximum in case of emission feature
C
23026 continue
23027 continue
      il = cl2 + 1
      ir = cr1 - 1
195   if(rflibc(il) .eq. -1.23e+34)then
C find first non-deleted point
      il = il + 1
      if(il .gt. ir)then
      write (ttyout, 196)
196   format (' ERROR in band map setup:',/, '       can not find a min 
     *or max: ', 'all channels deleted')
      ier = 1
      return
      endif
      go to 195
      endif
      rmin = rflibc(il)
      rmax = rmin
      minch = il
      maxch = il
      do23036 i = il, ir 
      if(rflibc(i) .eq. delpt)then
      goto 23036
      endif
      if(rflibc(i) .lt. rmin)then
      minch = i
      rmin = rflibc(i)
      endif
      if(rflibc(i) .gt. rmax)then
      maxch = i
      rmax = rflibc(i)
      endif
23036 continue
23037 continue
C band depth for an emission feature
      depth = 1.0 - rmin
C emission feature
      emiss = rmax - 1.0
      if(rmin .lt. 0.0)then
      write (ttyout, 200)
200   format (' ERROR: band depth in the library spectrum ',/, '        
     *is LESS THAN ZERO: that is invalid')
      ier = 1
      return
C
C determine if the feature is emission or absorption
C
      endif
      ifeattype = 0
      if(emiss .gt. depth)then
      write (ttyout, 201)
201   format (' NOTE: this feature is an emission feature.')
C emission feature
      ifeattype = -1
      else
C absorption band
      ifeattype = 1
      endif
      if(ifeattype .eq. -1 .and. maxch .eq. 0)then
      write (ttyout,303) maxch
303   format ('ERROR: emission feature maximum channel=', i7)
      ier = 1
      return
      endif
      if(ifeattype .eq. 1 .and. minch .eq. 0)then
      write (ttyout,304) minch
304   format ('ERROR: absorption feature minimum channel=', i7)
      ier = 1
      return
C
C DEBUG:
C
C	write (ttyout,*) 'DEBUG: bdmset: cl1=',cl1,' cl2=',cl2,
C					' cr1=',cr1,' cr2=',cr2
C	do i = 1, cr2 {
C		write (ttyout,*) 'DEBUG: bdmset: wav(',i,')=,wav(i),
C					'  rflib(',i,')=,rflib(i),
C					'  rflibc(',i,')=,rflibc(i)
C	}
C	write (ttyout,*) 'DEBUG: bdmset: minch=',minch,' ier=',ier
C	write (ttyout,*) 'DEBUG: bdmset: maxch=',maxch
C END.DEBUG
C
C  done!
C
      endif
      return
      end
