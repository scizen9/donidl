pro wfpc2list,searchspec,outputfile
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) eq 0) then begin
    print,"Call> wfpc2list,search_spec,[outputfile]"
    print,"e.g.> wfpc2list,'*.c0h'"
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
  
    if (i/30 eq i/30.) then begin
      if (i ne 0) then printf,2,''
      printf,2,'FILENAME   FILT1  EXPTIME  DATE-OBS  TIME-OBS  TARGNAME'
      printf,2,'---------  -----  -------  --------  --------  --------------'
;     printf,2,'u2kl0405t  F218W  1200.00  20/02/95  22:10:16  NGC6624'
    endif

    sxhread,files(i),h
  
    filename=strmid(files(i),0,strlen(files(i))-4)
    if (strlen(filename) gt 10) then filename=strmid(filename,strlen(filename)-10,99)
    if (strpos(filename,'/') ne -1) then filename=strmid(filename,strpos(filename,'/')+1,199)
    filename=filename+pad
    print,filename

    naxis1=sxpar(h,'NAXIS1')
    naxis2=sxpar(h,'NAXIS2')
    filtnam1=strn(sxpar(h,'FILTNAM1'))+pad
    exptime=sxpar(h,'EXPTIME')
    dateobs=strn(sxpar(h,'DATE-OBS'))
    timeobs=strn(sxpar(h,'TIME-OBS'))
    object=strn(sxpar(h,'TARGNAME'))+pad

    printf,2,format='(a9,2x,a5,2x,f7.2,2x,a8,2x,a8,2x,a20)', $
      filename,filtnam1,exptime,dateobs,timeobs,object  
    endfor
  
  close,2

end
