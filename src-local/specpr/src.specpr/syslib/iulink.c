/* fortran 77 interface to unlink system call */

/* RED - Added to following include on RH5 64bit to eliminate TLS mismatch error */
#include <errno.h>
int syserr_;
extern errno;

/*  machine dependency added 3/27/85. Roger Clark */

iulink(f1,l1)

char *f1; long l1;
{
	char buf[256];
	int i;
	
	for (i=0; i<sizeof(buf)-1 && f1[i]!=' ' && i<l1; i++)
		buf[i] = f1[i];
	buf[i] = '\0';
	
	i = unlink(buf);
	if (i!=0) syserr_ = errno;
	else syserr_ = 0;
	return(i);
}

/* below is identical to above except _ added */

iulink_(f1,l1)

char *f1; long l1;
{
	char buf[256];
	int i;
	
	for (i=0; i<sizeof(buf)-1 && f1[i]!=' ' && i<l1; i++)
		buf[i] = f1[i];
	buf[i] = '\0';
	
	i = unlink(buf);
	if (i!=0) syserr_ = errno;
	else syserr_ = 0;
	return(i);
}
