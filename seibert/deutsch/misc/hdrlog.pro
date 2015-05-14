;+
; *****************************************************************************
; ******* PROCEDURE: HDRLOG
; *****************************************************************************
; *** DESCRIPTION:
; ***	  This procedure takes a group of files in the SDAS format and creates
; ***	a .LOG file of concatenated headers in the exact same format as the
; ***	FITSLOG program does with FITS tapes.
; *** INPUT:
; ***	SRCHPATH  The path specification and filter where to search for the
; ***		header files.  MUST include the .xxH extender!
; ***	LOGfile	  The output filename where the concatenated headers are put.
; *** OUTPUT:
; ***	A file of concatenated headers name LOGfile
; ***	All passed variables remain unchanged
; *** COMMON blocks:
; ***   None
; *** Other Procedures/Functions Called:
; ***   None
; *** NOTES:
; ***	None
; *** HISTORY:
; ***	08-AUG-90 Version 1 adapted from olde IDL version
; *********** IDL 2.0 ************ Eric W. Deutsch ********* 08-AUG-90 ********
;-

pro hdrlog,srchpath,LOGfile

  arg=n_params(0)
  if (arg lt 1) then begin
    print,'Call: IDL> HDRLOG,srchpath,LOGfile'
    print,"e.g.: IDL> HDRLOG,'DISK$DATA1:[DEUTSCH.BACK]*.HHH','TARGS.LOG'"
    print,"e.g.: IDL> HDRLOG,'/u1/*.hhh','targs.log'"
    goto,BRK
    endif

  files=findfile(srchpath)
  nfiles=n_elements(files)

  openw,2,LOGfile

  for i=0,nfiles-1 do begin
    file=files(i)
    j=0
    if (!version.os eq 'vms') then begin
      while (strmid(file,j,1) ne ';') do j=j+1
      file=strmid(file,0,j)
      endif
    print,'Processing file: ',file
    sxhread,file,h
    printf,2,' '
    printf,2,'$(a,i3)','----------------------- file     ',i+1
    printf,2,' '

    tmp=sxpar(h,'OBJECT')
    if (string(tmp) eq string(0)) then begin
      j=0
      while (strmid(file,j,1) ne ']') do j=j+1
      file=strmid(file,j+1,strlen(file)-j)
      j=0
      while (strmid(file,j,1) ne '.') do j=j+1
      file=strmid(file,0,j)
      sxaddpar,h,'OBJECT',file,' Source filename'
      endif

    for j=0,n_elements(h)-1 do printf,2,h(j)
    endfor

  close,2

BRK:
  return
end
