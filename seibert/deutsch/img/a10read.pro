pro a10read,a10file,data,brighterthan=brighterthan
;+
; NAME:
;	A10READ
;
; PURPOSE:
;	Compare coordinates of stars in a GSC (gsclist) file and a USNO-A1.0
;	(a10list) file.
;
; CATEGORY:
;	Catalog software
;
; CALLING SEQUENCE:
;	a10read,a10file,data
;
; INPUTS:
;	a10file: The filename of an USNO-A1.0 extraction
;		which contains the target field and the surrounding region.
;
; OUTPUTS:
;	data:	Array containing A1.0 catalog data.  e.g.:
;	 RA (deg)  2000  Dec (deg)  B mag  R mag  Field  GSC?  Err?  Zone
;	 ------------  -----------  -----  -----  -----  ----  ----  ----
;	   267.148225   -37.177994   22.8   18.1    393     0     0   525
;	   267.148233   -37.326064   23.9   18.7    393     0     0   525
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	01/03/97 Written by E. Deutsch
;	01/14/98 Modified to autodetect and also read HTML format.  E. Deutsch
;
;-


; -- Not enough parameters?  Show the call sequence -------------------
  if (n_params(0) lt 2) then begin
    print,'Call> a10read,a10file,data'
    print,"e.g.> a10read,'KA2.a10list',data"
    return
    endif


  if (n_elements(brighterthan) eq 0) then brighterthan=99


  if (not exist(a10file)) then begin
    print,'ERROR: '+a10file+' not found.'
    return
    endif


  openr,1,a10file
  lin='' & readf,1,lin & fmt=1
  if (strn(lin) eq '<html>') or (strpos(lin,'<body') ne -1) then fmt=2
  if (strn(lin) eq 'RA(J2000)     DEC(J2000)      Magnitude') then fmt=3


  if (fmt eq 1) then begin
    spawn,'wc '+a10file,results
    tmp1=lonarr(3) & reads,results,tmp1 & nlines=tmp1(0)
    readf,1,lin
    data=dblarr(8,nlines) & d1=dblarr(8) & i=0L & i2=0L
    while not EOF(1) do begin
      readf,1,d1
      if (max(d1(2:3)) lt brighterthan) then begin
        data(*,i)=d1 & i=i+1 & endif
      i2=i2+1
      endwhile
    endif


  if (fmt eq 2) then begin
    spawn,'wc '+a10file,results
    tmp1=lonarr(3) & reads,results,tmp1 & nlines=tmp1(0)
    data=dblarr(8,nlines) & d1=dblarr(8) & i=0L & i2=0L
    while (not EOF(1) and (strpos(lin,"r_mag") eq -1)) do readf,1,lin
    while not EOF(1) do begin
      readf,1,lin
      if (strmid(lin,0,4) eq '</b>') then lin=strmid(lin,4,199)
      if (strn(lin) eq '') then goto,DONE
      if (strpos(lin,'PRE') ne -1) then goto,DONE
      lin=strmid(lin,0,67)+'1'
      reads,lin,format='(6x,f4,10x,f9,f10,f5,f6,f4,f9,f5)',d1
      if (d1(7) lt 0) then d1(7)=0 else d1(7)=1
      d1=d1([1,2,3,4,6,7,5,0])
      if (max(d1(2:3)) lt brighterthan) then begin
        data(*,i)=d1 & i=i+1 & endif
      i2=i2+1
      endwhile
    endif


  if (fmt eq 3) then begin
    spawn,'wc '+a10file,results
    tmp1=lonarr(3) & reads,results,tmp1 & nlines=tmp1(0)
    readf,1,lin & readf,1,lin
    data=dblarr(8,nlines) & d1=dblarr(8) & i=0L & i2=0L
    while not EOF(1) do begin
      readf,1,lin
      coords=strmid(lin,0,30)
      stringad,coords,ra,dec
      tmp1=getopt(strmid(lin,30,16),'F',3)
      d1(0)=[ra,dec,tmp1(1),tmp1(0),tmp1(2)]
      if (max(d1(2:3)) lt brighterthan) then begin
        data(*,i)=d1 & i=i+1 & endif
      i2=i2+1
      endwhile
    endif


DONE:

  close,1
  if (i gt 0) then data=data(*,0:i-1) else data=-1
  print,strn(i2),' objects in file'
  print,strn(i),' objects selected'

  return

end



