pro unred,wave,flux,ebv,funred,table,SILENT=silent
;+
; NAME:
;   UNRED
; PURPOSE:
;   Correct a flux vector for interstellar extinction based on the the
;   results of Cardelli, Clayton, & Mathis 1989, ApJ, 345, 245
; CALLING SEQUENCE:
;   UNRED,wave,flux,ebv,funred      
; INPUT:
;   WAVE - wavelength vector (Angstroms)
;   FLUX - calibrated flux vector, same number of elements as WAVE
;   EBV  - color excess E(B-V), scalar.  If a negative EBV is supplied,
;          then fluxes will be reddened rather than deredenned.
; OUTPUT:
;   FUNRED - unreddened flux vector, same units and number of elements
;            as FLUX
; REVISION HISTORY:
;   1999-02-09: Rewrote UIT procedure of the same name to just use
;               the new results of Cardelli et al. (1989) by E. Deutsch
;-

  if (n_params() lt 4) then begin
    print,'Syntax: UNRED,wave,flux,ebv,funred'
    return
    endif

  if (ebv eq 0.0) then begin            ;No change in flux if E(B-V) = 0
    funred = flux                   
    return
    endif

  R_V=3.1
  x=1/(wave*1d-10/1d-6)
  AloAv=x*0.0d

  sel=where(x lt 0.3)
  if (sel(0) ne -1) then print,'[Warning] UNRED: Extrapolating beyond IR limit'

  sel=where(x lt 1.1)
  if (sel(0) ne -1) then begin
    y=x(sel)
    a=0.574*y^1.61
    b=-0.527*y^1.61
    AloAv(sel)=a+b/R_V
    endif

  sel=where((x ge 1.1) and (x lt 3.3))
  if (sel(0) ne -1) then begin
    y=x(sel)-1.82d
    a=1 + 0.17699*y^1 - 0.50447*y^2 - 0.02427*y^3 + 0.72085*y^4 + 0.01979*y^5 $
        - 0.77530*y^6 + 0.32999*y^7
    b=    1.41338*y^1 + 2.28305*y^2 + 1.07233*y^3 - 5.38434*y^4 - 0.62251*y^5 $
        + 5.30260*y^6 - 2.09002*y^7
    AloAv(sel)=a+b/R_V
    endif

  sel=where((x ge 3.3) and (x lt 5.9))
  if (sel(0) ne -1) then begin
    y=x(sel)
    a= 1.752 - 0.316*y - 0.104/((y-4.67)^2 + 0.341)
    b=-3.090 + 1.825*y + 1.206/((y-4.62)^2 + 0.263)
    AloAv(sel)=a+b/R_V
    endif

  sel=where((x ge 5.9) and (x lt 8.0))
  if (sel(0) ne -1) then begin
    y=x(sel)
    Fa= -0.04473*(y-5.9)^2 - 0.009779*(y-5.9)^3
    Fb=  0.2130 *(y-5.9)^2 + 0.1207  *(y-5.9)^3
    a= 1.752 - 0.316*y - 0.104/((y-4.67)^2 + 0.341) + Fa
    b=-3.090 + 1.825*y + 1.206/((y-4.62)^2 + 0.263) + Fb
    AloAv(sel)=a+b/R_V
    endif

  sel=where(x gt 10.0)
  if (sel(0) ne -1) then print,'[Warning] UNRED: Extrapolating beyond UV limit'

  sel=where(x ge 8.0)
  if (sel(0) ne -1) then begin
    y=x(sel)
    a= -1.073 - 0.628*(y-8) + 0.137*(y-8)^2 - 0.070*(y-8)^3
    b= 13.670 + 4.257*(y-8) - 0.420*(y-8)^2 + 0.374*(y-8)^3
    AloAv(sel)=a+b/R_V
    endif

  funred = flux * mag2flux(-AloAv*R_V*ebv,0)

  return

end                          
