pro GetDefVal,value,keyword
;+
; NAME:
;   GETDEFVAL
; PURPOSE:
;   This procedure returns the default value for the supplied keyword, first
;   checking IDL_PRFDEF and then IDL_SYSDEF.
; CALLING SEQEUNCE:
;   GetDefVal,value,keyword
;-


  DEF_FILE=getenv('IDL_PRSDEF')
  if (DEF_FILE ne '') then begin
    if exist(DEF_FILE) then begin
      sxhread,DEF_FILE,DEF
      tmp=sxpar(DEF,keyword)
      if not !ERR then begin & value=tmp & return & endif
      endif
    endif

  DEF_FILE=getenv('IDL_SYSDEF')
  if (DEF_FILE ne '') then begin
    if exist(DEF_FILE) then begin
      sxhread,DEF_FILE,DEF
      tmp=sxpar(DEF,keyword)
      if not !ERR then begin & value=tmp & return & endif
      endif
    endif

  return
end
