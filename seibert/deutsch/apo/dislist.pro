pro dislist,searchspec,outputfile
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) eq 0) then begin
    print,"Call> dislist,search_spec,[outputfile]"
    print,"e.g.> dislist,'*r.hhh'"
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
      printf,2,' FILENAME    DATATYPE    IMAGESIZE    TURRET    EXPTIME     UT        RA          DEC     EPCH  ROT  AIRMS  IMTYPE FLT  OBJECT NAME'
      printf,2,'----------  ----------  -----------  ---------  -------  --------  ----------  ---------  ----  ---  -----  ------  -  -------------'
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
    turret=strn(sxpar(h,'TURRET'))+pad
    exptime=sxpar(h,'EXPOSURE')
    ut=strn(sxpar(h,'UT'))
    ra=strn(sxpar(h,'RA'))
    dec=strn(sxpar(h,'DEC'))
    epoch=sxpar(h,'EPOCH')
    rot=sxpar(h,'ROTATION')
    airmass=sxpar(h,'AIRMASS')
    imtype=strn(sxpar(h,'IMAGETYP')) & if (imtype eq '0') then imtype=''
    filters=strn(sxpar(h,'FILTERS')) & if (filters eq '0') then filters=''
    object=strn(sxpar(h,'OBJECT'))
    if (object eq '0') or (object eq '') then object=strn(sxpar(h,'OBJNAME'))
    if (object eq '0') or (object eq '') then object=strn(sxpar(h,'COMMENT'))
    if (object eq '0') then object=''
    if (strmid(object,0,3) eq "= '") then object=strmid(object,3,strlen(object)-4)
    imtype=imtype+pad
    object=object+pad

    printf,2,format='(a10,2x,a10,2x,a11,2x,a9,2x,f7.1,2x,a8,2x,a10,2x,a9,2x,'+$
      'i4,1x,i4,2x,f5.3,2x,a6,2x,a1,2x,a13)', $
      filename,datatype,vect([NAXIS1,NAXIS2])+pad,turret,exptime,ut,ra,dec, $
      epoch,rot,airmass,imtype,filters,object
  
    endfor
  
  close,2

end
