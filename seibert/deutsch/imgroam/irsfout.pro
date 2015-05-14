pro IRSFout,file,ss,XY=XY,RADEC=RADEC,ID=ID
;+
; NAME:
;   IRSFout
; PURPOSE:
;   Convert an IRSF structure into a regular list file (e.g. for imalign)
; CALLING SEQEUNCE:
;   IRSFout,filename,ss,[/XY,/RADEC,/ID]
; INPUT:
;   FILENAME  The filename of the output file (e.g. stars.lis
;   SS        The 'Saved_Stars' structure from which the file is generated.
; OPTIONAL INPUT:
;   XY        If set, the X and Y coordinates are written.
;   RADEC     If set, the RA and DEC coordinates are written.
;   ID        If set, the ID's are written.
; HISTORY:
;   15-JAN-95 Written by Eric W. Deutsch
;   18-DEC-96 Added capability to write ID field.  Eric W. Deutsch
;-

  arg=n_params(0)
  if (arg lt 2) then begin
    print,'CALL> IRSFout,output_file_name,Selected_Stars_structure,[/XY,/RADEC]'
    print,"e.g.> IRSFLoad,'M81.lis',ss,/XY"
    return
    endif

  if (n_elements(XY) eq 0) then XY=0
  if (n_elements(RADEC) eq 0) then RADEC=0
  if (n_elements(ID) eq 0) then ID=0
  if (XY eq 0) and (RADEC eq 0) and (ID eq 0) then XY=1

  openw,1,file
  for i=0,ss.stars-1 do begin
    out=0.0
    if (RADEC eq 1) then out=[out,ss.RA(i),ss.DEC(i)]
    if (XY eq 1) then out=[out,ss.X(i),ss.Y(i)]
    if (ID eq 1) then out=[out,ss.ID(i)]
    if (n_elements(out) gt 1) then out=out(1:n_elements(out)-1)
    printf,1,out
    endfor

  close,1
  return
end
