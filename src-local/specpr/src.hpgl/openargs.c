#include	"hpdaemon.h"
/* RED - Added to following include on RH5 64bit to eliminate TLS mismatch error */
#include <errno.h>
/*
 *	@(#)openargs.c	2.2 01/10/96 16:19:43
 */


static char Sccsid[]="@(#)openargs.c	2.2 01/10/96 16:19:43";

extern errno;

openargs()
{
	char line[128];
	int	error;

	copies = 1;

	while (gets(line) != NULL) {
		switch (line[0]) {
		case 'T':
			errno = 0;
			text = fopen(&line[1],"r");
			logerr("Text = %s\n", &line[1]);
			break;
		case 'V':
			errno = 0;
			vector = fopen(&line[1],"r");
			logerr("Vector = %s\n", &line[1]);
			break;
		case 'C':
			copies = atoi(&line[1]);
			if (copies<1 || copies>9) copies=1;
			if (vector==(FILE *)NULL || text==(FILE *)NULL) {
				logerr("Can't open input files vector=%d, text=%d\n",vector,text);
				exit(BAD_INPUT_FILE);
			}
			return (1);
		default:
			break;
		}
	}
	if (vector!=NULL) fclose(vector);
	if (text!=NULL) fclose(text);
	return (0);
}
