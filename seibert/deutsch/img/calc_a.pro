pro Calc_A,wavel,EBmV,value=value,silent=silent
;+
; NAME:
;   CALC_A
; PURPOSE:
;   Returns the extinction of the supplied wavelength and E(B-V) using the
;   Seaton Extinction curve through the UNRED procedure.
; CALLING SEQEUNCE:
;   Calc_A,wavelenth,[EBmV],[value=value]
; INPUT:
;   WAVELENTH This must be the scalar wavelength for which the extinction
;               is to be calculated (in Angstroms).
; OPTIONAL INPUT:
;   EBmV      This scalar contains the optional value for E(B-V).  If this
;               value is not supplied the factor times which E(B-V) should
;               be multiplied by is returned.
;   SILENT    This Keyword, if set, prevents the result from being displayed.
; OPTIONAL OUTPUT:
;   VALUE     If this keyword is supplied the value is returned into this
;               variable.
; HISTORY:
;   17-NOV-92 Header added to old routine  (E. Deutsch)
;-

  if (n_elements(silent) eq 0) then silent=0

  arg=n_params(0)
  if (arg lt 1) then begin
    print,'Calling Sequence: Calc_A,wavelenth,[EBmV],[value=value]'
    return
    endif
  if (arg lt 2) then EBmV=1.0

  UNRED,[wavel],[1],EBmV,F,2,silent=silent

  if (arg lt 2) then begin
    value=-1.*(flux2mag(F(0))-flux2mag(1))/EBmV
    if (not silent) then $
      print,'Extinction is ',strn(value),'*E(B-V) at ',strn(wavel),'A'
    endif

  if (arg eq 2) then begin
    value=-1.*(flux2mag(F(0))-flux2mag(1))/1.0
    if (not silent) then $
      print,'Extinction is ',strn(value),' at ',strn(wavel),'A'
    endif

end
