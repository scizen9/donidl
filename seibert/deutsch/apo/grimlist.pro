pro grimlist,searchspec,outputfile
;+
; NAME:
;	GRIMLIST
;
; PURPOSE:
;	Generate a listing of GRIM images with interesting information in
;	various columns.
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	grimlist,searchspec
;	grimlist,searchspec [,outputfilename]
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
;	grimlist,'*.hhh'
;
; MODIFICATION HISTORY:
;	1995 Written by E. Deutsch
;
;-

  if (n_params(0) lt 1) then begin
    print,"Call> grimlist,search_spec"
    print,"Call> grimlist,search_spec [,outputfile]"
    print,"e.g.> grimlist,'*.hhh'"
    return
    endif


  files=findfile(searchspec)
  nfiles=n_elements(files)
  if (nfiles eq 1) and (files(0) eq '') then begin
    print,'Unable to find any files with search spec: ',searchspec
    return
    endif

  if (n_elements(outputfile) eq 0) then outputfile='listhdrs.out'

  print,'Writing to file '+outputfile+'...'
  openw,2,outputfile
  pad='                 '

  for i=0, nfiles-1 do begin
  
    if (i/42 eq i/42.) then begin
      if (i ne 0) then printf,2,''
      printf,2,' FILENAME    DATATYPE    IMAGESIZE   LENS  EXPTIME     UT        RA          DEC     ROT  AIRMS  FLT1  FLT2  GRS  SLIT   OBJECT NAME'
      printf,2,'----------  ----------  -----------  ----  -------  --------  ----------  ---------  ---  -----  ----  ----  ---  ----  -------------'
    endif else if (i/6 eq i/6.) then printf,2,''


    sxhread,files(i),h
  
    filename=strmid(files(i),0,strlen(files(i))-4)
    if (strlen(filename) gt 10) then filename=strmid(filename,strlen(filename)-10,99)
    if (strpos(filename,'/') ne -1) then filename=strmid(filename,strpos(filename,'/')+1,199)
    filename=filename+pad
    print,filename

    datatype=strn(sxpar(h,'DATATYPE'))+pad
    naxis1=sxpar(h,'NAXIS1')
    naxis2=sxpar(h,'NAXIS2')
    lens=sxpar(h,'LENS')
      lens=(['?','F/5','?','F/10','?','F/20','?'])(lens>0<5)
    exptime=sxpar(h,'OPENTIME')
    ut=strn(sxpar(h,'UT'))
    ra=strn(sxpar(h,'RA'))
    dec=strn(sxpar(h,'DEC'))
    rot=sxpar(h,'ROTATION')
    airmass=sxpar(h,'AIRMASS')

    flt1=sxpar(h,'FILTER1')
      flt1=(['?','J','H','K','K''','K sh','K co','K da','1.58','1.70','?','?','?','?'])(flt1>0<13)+pad
    flt2=sxpar(h,'FILTER2')
      if (strn(flt2) eq '0') then flt2='DARK'
      if (strn(flt2) eq '13') then flt2='OPEN'
      flt2=strn(flt2)+pad
    grism=sxpar(h,'GRISM')
      grism=(['?','?','OUT','IN','?','ND3','N13','N25','?'])(grism>0<8)+pad
    slit=sxpar(h,'SLIT')
      slit=(['?','240','120','60lo','60mi','60sh','?','NONE','?'])(slit>0<8)+pad

    object=strn(sxpar(h,'OBJECT'))
    if (object eq '0') or (object eq '') then object=strn(sxpar(h,'OBJNAME'))
    if (object eq '0') or (object eq '') then object=strn(sxpar(h,'COMMENT'))
    if (object eq '0') then object=''
    if (strmid(object,0,3) eq "= '") then object=strmid(object,3,strlen(object)-4)
    object=object+pad

    printf,2,format='(a10,2x,a10,2x,a11,2x,a4,2x,f7.1,2x,a8,2x,a10,2x,a9,2x,'+$
      'i3,2x,f5.3,2x,a4,2x,a4,2x,a3,2x,a4,2x,a13)', $
      filename,datatype,vect([NAXIS1,NAXIS2])+pad,lens,exptime,ut,ra,dec, $
      rot,airmass,flt1,flt2,grism,slit,object
  
    endfor
  
  close,2

end
