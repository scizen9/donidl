 
;*******************************************************************************
pro mpgaussfit, wavescale, flux, err, continuum, cerr, lineinfo, $
                linedata = linedata, fit = fit, noplot = noplot
;*******************************************************************************

;*******************************************************************************
; Compute equivalent width
;*******************************************************************************

continsub = flux - continuum
normflux = flux / continuum
nerr = sqrt((err / flux)^2 + (cerr / continuum)^2) * normflux

dw = wavescale - shift(wavescale, 1) 
dw[0] = dw[1]-(dw[2]-dw[1])

eqwidth=total((1.0 - normflux) * dw)
eqwidth_err=sqrt(total(nerr^2 * dw^2))

total_err = sqrt(err^2. + cerr^2.)

;*******************************************************************************
; Define fit limits & starting coefs for emission & absorption lines
;*******************************************************************************
dofit:

parinfo = replicate({value: 0.0D, fixed: 0, limited: [1,1], $
                     limits:[0.0D, 0.0D], tied: ""}, 6)

parinfo(0).value = abs(max(continsub))                  ; starting ampl
parinfo(0).limits(0)  = 0                               ; ampl min
parinfo(0).limits(1)  = abs(max(continsub))*1.6 + 0.1   ; ampl max
parinfo(1).value = lineinfo.centr                       ; starting line center
parinfo(1).limits(0)  = lineinfo.centr - 3.0            ; line center min
parinfo(1).limits(1)  = lineinfo.centr + 3.0            ; line center max
parinfo(2).value = 2.5                                  ; starting fwhm
parinfo(2).limits(0)  = 1.0                             ; fwhm min
parinfo(2).limits(1)  = 15.0                            ; fwhm max

parinfo(3).value = -1.0*abs(min(continsub))             ; starting ampl
parinfo(3).limits(0)  = abs(min(continsub))*(-1.6)-0.1  ; ampl min
parinfo(3).limits(1)  = 0                               ; ampl max
parinfo(4).value = lineinfo.centr                       ; starting line center
parinfo(4).limits(0)  = lineinfo.centr - 8.0            ; line center min
parinfo(4).limits(1)  = lineinfo.centr + 8.0            ; line center max
parinfo(5).value = 10.0                                 ; starting fwhm
parinfo(5).limits(0)  = 3.0                             ; fwhm min
parinfo(5).limits(1)  = 30.0                            ; fwhm max

if (lineinfo.type eq 'em') then begin
  parinfo([3,5]).limits = 0
  parinfo([3,5]).value = 0
  parinfo([3,5]).fixed = 1
  parinfo(4).tied = 'P(1)'
endif
if (lineinfo.type eq 'abs') then begin
  parinfo([0,2]).limits = 0
  parinfo([0,2]).value = 0
  parinfo([0,2]).fixed = 1
  parinfo(1).tied = 'P(4)'
endif

;*******************************************************************************
; Perform fit on spectrum   
;*******************************************************************************

wavescale = wavescale * 1.D
continsub = continsub * 1.D
total_err = total_err * 1.D
gcoef = parinfo(*).value * 1.D
gcoeflast = gcoef * 0
itera = 0

while total(gcoef - gcoeflast) ne 0.0 do begin

  gcoeflast = gcoef

  gfit = mpcurvefit(wavescale, continsub, 1.0/total_err^2, gcoef, sigma, $
         function_name = 'doublegauss', parinfo = parinfo, errmsg = errmsg, $
         iter = niter, status = status, covar = covar, chisq = chisq, $
         autoderivative = 0, /quiet)

  parinfo.value = gcoef 
  itera = itera + niter
  if itera gt 300 then gcoeflast = gcoef
endwhile

;*******************************************************************************
; Compute errors on fitted parameters the curvefit way   
;*******************************************************************************

weights = 1.0 / total_err^2
doublegauss, wavescale, gcoef, y, pder
nterms = n_elements(gcoef)            ; # of parameters
diag = lindgen(nterms)*(nterms+1)     ; Subscripts of diagonal elements
alpha = transpose(pder) # (Weights # (fltarr(nterms)+1)*pder)
sigma = sqrt( 1.0 / alpha[diag] )

sigma_bad=where(finite(sigma) EQ 0,cnt)     ;clean up the zero pder
IF cnt GE 1 THEN sigma[sigma_bad]=0 

;*******************************************************************************
; Compute the S/N of the fit
;*******************************************************************************
 
gausspars, gcoef[0:2], sigma[0:2], struct = em
gausspars, gcoef[3:5], sigma[3:5], struct = abs
em_sigma = abs(em.flux / em.flux_err)
abs_sigma = abs(abs.flux / abs.flux_err)

if lineinfo.type eq 'em' then nsigma = em_sigma
if lineinfo.type eq 'abs' then nsigma = abs_sigma
if lineinfo.type eq 'both' then BEGIN
  IF em_sigma le 1.0 AND abs_sigma gt 1.0 THEN lineinfo.type = 'abs'
  IF em_sigma gt 1.0 AND abs_sigma le 1.0 THEN lineinfo.type = 'em'
  IF lineinfo.type NE 'both' THEN GOTO, dofit ;refit as a single componet fit
  nsigma = (em.flux - abs.flux) / sqrt(em.flux_err^2 + abs.flux_err^2)
  if em.flux eq 0 then nsigma = abs_sigma
  if abs.flux eq 0 then nsigma = em_sigma
  IF em_sigma le 1.0 AND abs_sigma le 1.0 THEN nsigma=0.
endif
   
IF strmid(lineinfo.line,0,2) eq 'H_' AND lineinfo.type NE 'both' THEN $
  lineinfo.type='both' ; so both blamer components are returned even if only
                       ; one is fit

;*******************************************************************************
; If not well fit then set coef to zero
;*******************************************************************************
 
if nsigma le 1.0 or not finite(nsigma) then begin
  gcoef[*] = 0.0
  sigma[*] = 0.0 
  fit = gfit * 0
  nsigma=0.
endif else fit = gfit

gausspars, gcoef[0:2], sigma[0:2], struct = em
gausspars, gcoef[3:5], sigma[3:5], struct = abs
if lineinfo.type eq 'em' or lineinfo.type eq 'both' then g = em else g = abs 

;*******************************************************************************
; Store  results of fit
;*******************************************************************************

tiny=1E-37 ; to reduce errors

linedata.eqwidth = eqwidth
linedata.eqwidth_err = eqwidth_err
cflux = continuum[value_to_index(wavescale, g.center)] 
linedata.continuum = cflux
linedata.cerr = cerr

for i = 0, 5 do linedata.(i + 4) = g.(i)
linedata.geqwidth = -1 * g.flux / (cflux + tiny) 
linedata.geqwidth_err = g.flux_err / (cflux + tiny) 
linedata.sn = nsigma

if lineinfo.type eq 'both' then begin
  linedata = [linedata, linedata]
  cflux = continuum[value_to_index(wavescale, abs.center)] 
  linedata[1].continuum = cflux
  for i = 0, 5 do linedata[1].(i + 4) = abs.(i)
  linedata[1].geqwidth = -1 * abs.flux / (cflux + tiny) 
  linedata[1].geqwidth_err = abs.flux_err / (cflux + tiny) 
endif

;*******************************************************************************
; Print results of fit
;*******************************************************************************

print, string(format = '(A11)', lineinfo[0].line) + $
       string(format = '(F9.2)', linedata[0].eqwidth )  + $
       string(format = '(F8.2)', linedata[0].eqwidth_err)  +  $
       string(format = '(F9.2)', linedata[0].geqwidth)  +  $
       string(format = '(F9.2)', linedata[0].flux) + $
       string(format = '(F10.2)', linedata[0].flux_err) + $
       string(format = '(F8.2)', linedata[0].fwhm) +  $
       string(format = '(F7.1)', nsigma) + $
       string(format = '(I6)', itera)

;*******************************************************************************

;*******************************************************************************
; Overplot fit and output nsigma
;*******************************************************************************

if not keyword_set(noplot) and nsigma gt 1.0 then begin

  if nsigma gt 1.0 then  fitcolor = !blue
  if nsigma ge 1.96 then fitcolor = !dgreen
  if nsigma ge 2.33 then fitcolor = !red
  
  IF lineinfo.type eq 'both' OR strmid(lineinfo.line,0,2) eq 'H_' THEN BEGIN
    e_nsigma=0.
    IF em.flux_err NE 0. THEN e_nsigma = abs(em.flux/em.flux_err)

    a_nsigma=0.
    IF abs.flux_err NE 0. THEN a_nsigma = abs(abs.flux/abs.flux_err)

    if (e_nsigma > a_nsigma) gt 1.0 then  fitcolor = !blue
    if (e_nsigma > a_nsigma) ge 1.96 then fitcolor = !dgreen
    if (e_nsigma > a_nsigma) ge 2.33 then fitcolor = !red

  endif

  oplot, wavescale - lineinfo.centr, fit + continuum, color=fitcolor, thick=2

  if lineinfo.type eq 'both' OR strmid(lineinfo.line,0,2) eq 'H_' THEN begin
     xyouts, -39, !y.crange[1]+3*(!y.crange[1]-!y.crange[0]) $
             / n_elements(!y.tickv), strn(e_nsigma,format='(f5.2)')+ $
             '!4r!x/'+strn(a_nsigma,format='(f5.2)')+'!4r!x', charsize=1.0
  endif else begin
    xyouts, -39, !y.crange[1]+3*(!y.crange[1]-!y.crange[0]) $
            / n_elements(!y.tickv), strn(nsigma,format='(f5.2)')+ $
            '!4r!x',charsize=1.0
  endelse

endif

end
