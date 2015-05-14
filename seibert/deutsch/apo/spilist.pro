pro spilist,searchspec,outputfile
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) eq 0) then begin
    print,"Call> spilist,search_spec,[outputfile]"
    print,"e.g.> spilist,'*.fits'"
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
  
    if (i/40 eq i/40.) then begin
      if (i ne 0) then printf,2,''
      printf,2,' FILENAME    IMAGESIZE   EXPTIME     UT        RA          DEC     EQNX  ROT  AIRMS  IMTYPE   FILTER   OBJECT NAME'
      printf,2,'----------  -----------  -------  --------  ----------  ---------  ----  ---  -----  ------  --------  ----------------'
    endif else if (i/5 eq i/5.) then printf,2,''

    sxhread,files(i),h
  
    filename=strmid(files(i),0,strlen(files(i))-5)
    if (strlen(filename) gt 10) then filename=strmid(filename,strlen(filename)-10,99)
    if (strpos(filename,'/') ne -1) then filename=strmid(filename,strpos(filename,'/')+1,199)
    filename=filename+pad
    print,filename

    naxis1=sxpar(h,'NAXIS1')
    naxis2=sxpar(h,'NAXIS2')
    exptime=sxpar(h,'EXPTIME')
    ut=strn(sxpar(h,'UT'))
    ra=strn(sxpar(h,'RA'))
    dec=strn(sxpar(h,'DEC'))
    epoch=sxpar(h,'EQUINOX')
    rot=sxpar(h,'ROTATE')
    airmass=sxpar(h,'AIRMASS')
    imtype=strn(sxpar(h,'IMAGETYP')) & if (imtype eq '0') then imtype=''
    filter=strn(sxpar(h,'FILTER')) & if (filter eq '0') then filter=''
    object=strn(sxpar(h,'OBJECT'))
    if (object eq '0') or (object eq '') then object=strn(sxpar(h,'COMMENT'))
    if (object eq '0') then object=''
    imtype=imtype+pad
    object=object+pad

    printf,2,format='(a10,2x,a11,2x,f7.1,2x,a8,2x,a10,2x,a9,2x,'+$
      'i4,1x,i4,2x,f5.3,2x,a6,2x,a8,2x,a13)', $
      filename,vect([NAXIS1,NAXIS2])+pad,exptime,ut,ra,dec, $
      epoch,rot,airmass,imtype,filter,object
  
    endfor
  
  close,2

end
