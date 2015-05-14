pro grimsummary,searchspec,outputfile
;+
; NAME:
;	GRIMSUMMARY
;
; PURPOSE:
;	Generate a summary of GRIM images with interesting information in
;	various columns.
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	grimsummary,searchspec
;	grimsummary,searchspec [,outputfilename]
;
; INPUTS:
;	searchspec: A string containing the search specification for the
;		images to be listed.  This is typically '*.hhh'
;
; OPTIONAL INPUTS:
;	outputfilename: A string containing the name of an output file
;		which will contain the listing of images.  If this
;		paramenter is not supplied, the name will be 'listhdrs.out'
;
; OPTIONAL INPUT KEYWORDS:
;	None.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	A list of images is assembled, and then each header is read and a
;	list of interesting information about each image is written to a list.
;
; EXAMPLE:
;	grimsummary,'*.hhh'
;
; MODIFICATION HISTORY:
;	1997-MAR-19  Written by E. Deutsch
;
;-

  if (n_params(0) lt 1) then begin
    print,"Call> grimsummary,search_spec"
    print,"Call> grimsummary,search_spec [,outputfile]"
    print,"e.g.> grimsummary,'*.hhh'"
    return
    endif


  files=findfile(searchspec)
  nfiles=n_elements(files)
  if (nfiles eq 1) and (files(0) eq '') then begin
    print,'Unable to find any files with search spec: ',searchspec
    return
    endif

  if (n_elements(outputfile) eq 0) then outputfile='listsummary.out'

  print,'Writing to file '+outputfile+'...'
  openw,2,outputfile
  pad='                 '

  filename='' & lens='' & exptime=0 & flt1='' & airmass=0.0 & object=''
  pfilename='' & plens='' & pexptime=0 & pflt1='' & pairmass=0.0 & pobject=''
  firstfilename='' & filctr=0 & psky=0.0 & sky=0.0

  for i=0,nfiles-1 do begin
  
    if (i eq 0) then begin
      if (i ne 0) then printf,2,''
      printf,2,'    FILENAMES       N    OBJNAME    EXPTIM  AIRMASS  LENS  FILT   SKY   TARGCTS'
      printf,2,'-----------------  ---  ----------  ------  -------  ----  ----  -----  -------'
;      printf,2,'n1.0001 - n1.0005  100  PG0824+289     3.9    1.234   F/5  K     10000'
    endif else if (i eq 9999) then printf,2,''


    sxhread,files(i),h
    grimread,img,h,files(i),/noflat,darkfile='Dark.hhh'
    skyline,img(*,100:105),sky,rms
    filctr=filctr+1

    filename=strmid(files(i),0,strlen(files(i))-4)
    if (strlen(filename) gt 7) then filename=strmid(filename,strlen(filename)-7,99)
    if (strpos(filename,'/') ne -1) then filename=strmid(filename,strpos(filename,'/')+1,99)
    filename=filename+pad
    print,filename

    lens=sxpar(h,'LENS')
      lens=(['?','F/5','?','F/10','?','F/20','?'])(lens>0<5)
    exptime=sxpar(h,'OPENTIME')
    airmass=sxpar(h,'AIRMASS')

    flt1=sxpar(h,'FILTER1')
      flt1=(['DARK','J','H','K','K''','K sh','K co','K da','1.58','1.70','?','?','?','?'])(flt1>0<13)+pad

    object=strn(sxpar(h,'OBJECT'))
    if (object eq '0') or (object eq '') then object=strn(sxpar(h,'OBJNAME'))
    if (object eq '0') or (object eq '') then object=strn(sxpar(h,'COMMENT'))
    if (object eq '0') then object=''
    if (strmid(object,0,3) eq "= '") then object=strmid(object,3,strlen(object)-4)
    object=object+pad

    if (lens ne plens) or (exptime ne pexptime) or (flt1 ne pflt1) or (object ne pobject) then begin
      if (plens ne '') then begin
        printf,2,format='(a7," - ",a7,2x,i3,2x,a10,2x,f6.1,2x,f7.3,2x,a4,2x,a4,2x,i5)', $
          firstfilename,pfilename,filctr-1,pobject,pexptime,pairmass,plens,pflt1,psky
        endif
      firstfilename=filename & plens=lens & pexptime=exptime & pflt1=flt1 & pobject=object
      pairmass=airmass
      filctr=1
      endif

    pfilename=filename & psky=sky
  
    endfor
  
  printf,2,format='(a7," - ",a7,2x,i3,2x,a10,2x,f6.1,2x,f7.3,2x,a4,2x,a4,2x,i5)', $
    firstfilename,pfilename,filctr-1,pobject,pexptime,pairmass,plens,pflt1,psky

  close,2

end
