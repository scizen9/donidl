pro IRSFSave,file,ss
;+
; NAME:
;   IRSFSave
; PURPOSE:
;   Save an IMGroam Star File (IRSF) specified by file from the structure.
; CALLING SEQEUNCE:
;   IRSFSave,filename,ss,/silent
; INPUT:
;   FILENAME  The filename of the IRSF file (it is highly recommended that
;               the file extensions are .irsf although it is not required.)
; OUTPUT:
;   SS        The 'Saved_Stars' structure which is saved.
; HISTORY:
;   10-DEC-92 Header finally added to this procedure which has been in use for
;     three or more years.  Name changed from GS_SFSave.  All versions
;     and header by Eric W. Deutsch
;-

  arg=n_params(0)
  if (arg lt 2) then begin
    print,'CALL> IRSFSave,file_name,Selected_Stars_structure'
    print,"e.g.> IRSFSave,'M81.IRSF',ss"
    return
    endif
  s=size(ss)
  if (s(2) ne 8) then begin
    print,'[IRSFSave] Error: Selected Stars Structure not defined.' & return
    endif
  if (ss.stars eq 0) then begin
    print,'[IRSFSave] Error: Selected Stars Structure contains no Stars.' & return
    endif

  openw,1,file
  printf,1,'IMGRoam Star File (Type 2)'
  printf,1,'Created on ',systime(0)
  printf,1,'Contains ',strn(ss.stars),' stars'
  printf,1,'ID #   Star ID      RA (deg)     DEC(deg)       X        Y       MAG'
  printf,1,'----  ----------  ------------ ------------  -------- --------  -----'

  fmt2='$(i4,2x,a10,1x,2f13.8,1x,2f9.3,f7.2)'
  for i=0,ss.stars-1 do $
    printf,1,fmt2,ss.id(i),ss.name(i),ss.ra(i),ss.dec(i),ss.x(i),ss.y(i), $
      ss.mag(i)

  close,1
  return
end
