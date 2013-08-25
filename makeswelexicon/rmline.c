// first argument: filename of list of line numbers to delete. files start with line 1.
// stdin: input file
// stdout: output file
// dqplz@yahoo.com / 5 Aug 2008
#include <stdio.h>

main(int ac, char *av[])
{
  FILE *in;

  int line=1;
  int del=0;
  int rc=0;
  unsigned char iw[100];
  if((in=fopen(av[1],"r"))==NULL) {
    fprintf(stderr,"%s: can not open '%s' for reading\n",av[0],av[1]);
    rc=1;
  }
  else {
    while(fgets(iw,sizeof(iw),stdin)!=NULL) {
      if(del==0||del<line) fscanf(in,"%d",&del);
      if(line++!=del) fputs(iw,stdout);
    }
    if(in) fclose(in);
  }
  return rc;
}
