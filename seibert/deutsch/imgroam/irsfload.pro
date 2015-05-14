pro IRSFLoad,file,ss,silent=silent,blank=blank
;+
; NAME:
;   IRSFLoad
; PURPOSE:
;   Load an IMGroam Star File (IRSF) specified by file into a structure.
; CALLING SEQEUNCE:
;   IRSFload,filename,ss,/silent
; INPUT:
;   FILENAME  The filename of the IRSF file (it is highly recommended that
;               the file extensions are .irsf although it is not required.)
; OUTPUT:
;   SS        The 'Saved_Stars' structure into which the file is read.
;               int          stars    Number of stars stored
;               intarr(1000)  ID       Some star number (sequential or not)
;               strarr(1000)  Name     String Name for star
;               dblarr(1000)  X        X Position of Star
;               dblarr(1000)  Y        Y Position of Star
;               dblarr(1000)  RA       RA Coordinate of Star
;               dblarr(1000)  DEC      DEC Coordinate of Star
;               fltarr(1000)  Mag      Magnitude of Star
; OPTIONAL INPUT:
;   SILENT    If set, the 'n stars loaded' message will not print.
;   BLANK     If set, then the filename is ignored and a blank structure
;               is returned.
; HISTORY:
;   10-DEC-92 Header finally added to this procedure which has been in use for
;     three or more years.  Name changed from GS_SFLoad.  All versions
;     and header by Eric W. Deutsch
;-

  arg=n_params(0)
  if (arg lt 2) then begin
    print,'CALL> IRSFLoad,file_name,Selected_Stars_structure,[/silent,/blank]'
    print,"e.g.> IRSFLoad,'M81.IRSF',ss"
    return
    endif
  if (n_elements(silent) eq 0) then silent=0

  ss={Selected_Stars1,stars:0,ID:intarr(1000),Name:strarr(1000),X:dblarr(1000), $
    Y:dblarr(1000),RA:dblarr(1000),DEC:dblarr(1000),MAG:fltarr(1000)}

  if (n_elements(blank) eq 0) then blank=0
  if (blank eq 1) then return

  lin='string' & type=1
  openr,1,file
  while (type lt 10) and (not EOF(1)) do begin
    readf,1,lin
    if (strpos(lin,'(Type 2)') gt 0) then type=2
    if (strmid(lin,0,4) eq '----') then type=type+10
    endwhile
  if (type lt 10) then begin
    print,'File: ',file,' is not is Image Roam Star File Format.  Unable to load.'
    ss.stars=0 & return
    endif
  type=type-10

  i=0
  ID=1 & name='' & RA=1.0D & DEC=1.0D & X=1.0D & Y=1.0D & mag=1.0
  fmt1='$(I5,2f11.6,f6.2,2f8.2)'
  fmt2='$(i4,2x,a11,2f13.8,1x,2f9.3,f7.2)'
  while (not EOF(1)) do begin
    if (type eq 1) then readf,1,fmt1,ID,RA,DEC,Mag,X,Y
    if (type eq 2) then readf,1,fmt2,ID,name,RA,DEC,X,Y,mag
    ss.ID(i)=ID & ss.Name(i)=name & ss.RA(i)=RA & ss.DEC(i)=DEC
    ss.X(i)=X & ss.Y(i)=Y & ss.MAG(i)=mag
    i=i+1
    endwhile

  close,1 & ss.stars=i
  if (silent ne 1) then print,strn(i),' stars loaded.'
  return
end
