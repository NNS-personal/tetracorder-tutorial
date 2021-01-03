	subroutine banscr (ititl)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  ratfor
#ccc
#ccc  short description:
#ccc               This subroutine writes titles & user instructions,
#ccc               draws axes and band outlines.
#ccc  algorithm description:
#ccc                       none
#ccc  system requirements: none
#ccc  subroutines called:
#ccc                    er,movabs,drwabs,sb
#ccc  argument list description:
#ccc     arguments: ititl
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#c  band normalization screen 1
#c
#c  write title & user instructions, draw axes & band outline
#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	include "../common/lundefs"
	character*(*) ititl
	character*80 outline 	# for xwindow writes

	call er
	write (outline,10) char(0)
	call gwrite(outline)
	write (outline,11) char(0)
	call gwrite(outline)
	write (outline,12) char(0)
	call gwrite(outline)
	write (outline,13) ititl, char(0)
	call gwrite(outline)
	write(outline,23) char(0)
	call gwrite(outline)
	write (outline,14) char(0)
	call gwrite(outline)
	write (outline,15) char(0)
	call gwrite(outline)
	write (outline,16) char(0)
	call gwrite(outline)
	write (outline,17) char(0)
	call gwrite(outline)
	write (outline,18) char(0)
	call gwrite(outline)
	write(outline,23) char(0)
	call gwrite(outline)
	write (outline,19) char(0)
	call gwrite(outline)
	write (outline,20) char(0)
	call gwrite(outline)
	write (outline,21) char(0)
	call gwrite(outline)
	write (outline,22) char(0)
	call gwrite(outline)
	call movabs (249*2,306*2)
	call drwabs (504*2,306*2)
	call movabs (440*2,306*2)
	call drwabs (440*2,122*2)
	call movabs (504*2,122*2)
	call drwabs (249*2,122*2)
	call movabs (313*2,122*2)
	call drwabs (313*2,306*2)
	call movabs (0,92*2)
	call sb(0)
	write (outline,25)
	call gwrite(outline)
	call movabs (0,249*2)
	call sb(0)
	return

10	format ('                             band normalization routine',a1)
11      format ( '                             ---- ------------- -------',a1)
12 	format (22x,a,a1)
13	format(' type:',a,a1)

14	format(' d  to delete a point',a1)
15	format(' i  to (re)insert a point',a1)
16	format(' m  to move band limits',a1)
17	format(' e  to exit without',a1)
18	format('      normalization',a1)

19	format(' b  to normalize and exit',a1)
20	format('      to crt plot routine',a1)
21	format(' u  to unnormalize and exit',a1)
22	format('    (data * previous norm. fac1or)',a1)
23	format(20(' '),a1)

25	format (50x,'channel no.',a1)

	end
