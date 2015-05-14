/*
** imcombine - Combine an image cube with an arbitrary algorithm (like median)
**
** Build (Solaris 2.x) with:
     cc -G -Kpic -c imcombine.c
     ld -G -o imcombine.so imcombine.o
**
** In IDL call with:
     imgcube=findgen(3,3,5) & mode=1L
     sz=size(imgcube)
     fimg=fltarr(sz(1),sz(2))
     result=call_external('imcombine.so','imcombine',imgcube,fimg,sz,mode)
**
**  NOTE: imgcube and fimg variables must be type float!
**  NOTE: mode variable must be type LONG!
**  mode=0L: mean
**  mode=1L: median
**  mode=[2L,1,1]: clipped mean: nlo=1, nhi=1
**  mode=[3L,1,1]: clipped mean: nlo=1, nhi=1 but ignores 0.00's
**
*/



#include <stdlib.h>
#include <stdio.h>

int imcombine();
int floatcompare();



/* ------------------------------------------------------------------------ */
int imcombine(int argc, void *argv[])
{
  int i,ix,iy,iz,*sz,*mode,ndim,xsz,ysz,zsz;
  float *imgcube,*fimg,sum,npts,*sortbuf;
  int debug=1;
  char err[]="[imcombine] - ERROR: ";

  imgcube=(float *)argv[0];
  fimg=(float *)argv[1];
  sz=(int *)argv[2];
  mode=(int *)argv[3];

  ndim=*sz;
  if (ndim!=3) {
    printf("%s ndim=%d. Must supply 3-D data cube.\n\r",err,ndim); return(-1); }
  xsz=*(sz+1);
  ysz=*(sz+2);
  zsz=*(sz+3);
  sortbuf=(float *)calloc(zsz,sizeof(float));


  if (debug) {
    printf("argc=%d\n\r",argc);
    printf("ndim=%d\n\r",ndim);
    printf("xsz=%d\n\r",xsz);
    printf("ysz=%d\n\r",ysz);
    printf("zsz=%d\n\r",zsz);
    printf("imgcube=[");
    for (i=0;i<5;i++) printf("%f,",*(imgcube+i));
    printf("...]\n\r");
    }

  for (iy=0;iy<ysz;iy++) {
    if (debug) printf("%d ",iy);
    for (ix=0;ix<xsz;ix++) {
      sum=0.0; npts=0.0;
      for (iz=0;iz<zsz;iz++) {
        npts++; *(sortbuf+iz)=*(imgcube+iz*xsz*ysz+iy*xsz+ix);
        sum+=*(sortbuf+iz); }
      if (*mode==0) *(fimg+iy*xsz+ix)=sum/npts;
      if (*mode==1) {
        qsort(sortbuf,zsz,sizeof(float),floatcompare);
        *(fimg+iy*xsz+ix)=*(sortbuf+zsz/2); }
      if (*mode==2 || *mode==3) {
        int nlo,nhi,start,stop;
        nlo=*(mode+1);
        nhi=*(mode+2);
        start=nlo;
        stop=zsz-1-nhi;
        if (start>stop) {
          printf("%s mode=2, nlo=%d & nhi=%d too large for zsz.\n\r",err,nlo,nhi);
          return(-1); }
        qsort(sortbuf,zsz,sizeof(float),floatcompare);
        sum=0.0; npts=0.0;
        if (*mode==2) {
          for (i=start; i<=stop; i++) { sum+=*(sortbuf+i); npts++; }
          *(fimg+iy*xsz+ix)=sum/npts; }
        if (*mode==3) {
          int goodlo=0;
          for (i=0; i<=zsz-1; i++) { if (*(sortbuf+i)==0.0) goodlo++; }
          start=goodlo+nlo;
          if (start>=stop) start=goodlo;
          if (start>stop) stop=zsz-1;
          if (start>stop) start=zsz-1;
          for (i=start; i<=stop; i++) { sum+=*(sortbuf+i); npts++; }
          *(fimg+iy*xsz+ix)=sum/npts; } }
      }
    }

  return(0);

}


/* ------------------------------------------------------------------------ */
static int floatcompare(float *i,float *j)
{
  if (*i > *j) return (1);
  if (*i < *j) return (-1);
  return (0);
}


