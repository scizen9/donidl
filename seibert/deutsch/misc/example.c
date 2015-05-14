/*
** example - CALL_EXTERNAL example
**
** Build with:
**   cc -G -Kpic -c example.c
**   ld -G -o example.so example.o
**
** In IDL:
**   x=findgen(10)
**   s=call_external('example.so','sum_array',x,n_elements(x),/f_value)
**   print,s
**
*/


#include <stdio.h>

float sum_array(int argc, void *argv[])
{

  int i;
  float *fp,s=0.0;

  printf("argc=%d\n\r",argc);
  for (i=0;i<argc;i++) printf("argv[%d]=%x\n\r",i,argv[i]);

  for (i=*(int *)argv[1],fp=(float *) argv[0]; i--;) s+=*fp++;

  return(s);

}


