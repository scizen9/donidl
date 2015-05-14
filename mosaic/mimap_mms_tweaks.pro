;+
; NAME:
;   MIMAP_MMS_TWEAKS
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   To match sky values in overlapping images using statistics of the
;   overlap region.  Images must be matched pixel by pixel
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   diff=mimap_sky_diff(image_names,sky=sky,status=status)
;
; INPUTS:
;    image_names - list of overlapping *pixel matched* images to tweak
;                  the background values of.
;
; OPTIONAL INPUTS:
;    sky - the sky value to converge to
;
; KEYWORD PARAMETERS:
;    ARG4 - the keyword that must have a value
;    SWITCH1 -  a switch that can be set
;
;
; OUTPUT:
;    Describe the returned result.
;
;    OUT1 - the other returned output
;
; COMMENTS:
;    Describe useful info
;
; REVISION HISTORY:
;    Mar 2008 - written, B. Johnson
;
;--------------------------------------------------------------------

FUNCTION mimap_mms_tweaks,imnames,outs,softening=softening, $
	ra=ra,dec=dec,ps=ps,natpix=natpix,size=size, $
	fast=fast,medium=medium,slow=slow

nim=n_elements(imnames)
maxiter=max([10,nim*3])

; are we too large to
; all fit in memory?
large=(size^2*nim gt 1E8)

diff=fltarr(nim,nim)
if keyword_set(tol) EQ 0 then tol=1E-7

iter=0
converged=0
subtweak=fltarr(nim)
totaltweak=subtweak

;get the initial difference matrix
print,'Getting initial sky diffs...'
diff=mimap_mms_diffsky(imnames,nover=nover,bad=bad)
;
; only reproject if baddies found
if keyword_set(medium) then begin
    if bad[0] ge 0 and not keyword_set(fast) then begin
	print,'Baddies encountered, must fix them...'
	print,'Reprojecting:'
	for i=0,nim-1 do begin
		mimap_swarp_config,ra=ra,dec=dec,ps=ps,size=size,ctype='MIN', $
			outimname=outs[i]
		call='swarp '+imnames[i]+' -c mimap.swarp'
		spawn,call
		print,i+1,'/',nim,imnames[i],' -> ',outs[i], $
			form='(i4,a1,i4,2x,a,a,a)'
	endfor
	spawn,'rm mimap.swarp'
	pixratio=ps[0]*ps[1]/natpix^2
	;
	; only recalculate baddies
	diff=mimap_mms_diffsky_slow(diff,nover,outs,bad, $
		large=large,pixratio=pixratio)
   endif
;
; slow method
endif else if keyword_set(slow) then begin
	print,'Reprojecting:'
	for i=0,nim-1 do begin
		mimap_swarp_config,ra=ra,dec=dec,ps=ps,size=size,ctype='MIN', $
			outimname=outs[i]
		call='swarp '+imnames[i]+' -c mimap.swarp'
		spawn,call
		print,i+1,'/',nim,imnames[i],' -> ',outs[i], $
			form='(i4,a1,i4,2x,a,a,a)'
	endfor
	spawn,'rm mimap.swarp'
	pixratio=ps[0]*ps[1]/natpix^2
	;
	; recalculate all diffs
	diff=mimap_mms_diffsky_slow(diff,nover,outs, $
		large=large,pixratio=pixratio)
endif
print,'Done getting initial sky diffs.'
;
; adjust tweaks until converged
while not converged do begin

;calculate subtweaks to apply to each image
  for i=0,nim-1 do begin
    neighbors=where(findgen(nim) NE i and nover[i,*] GT 0,numneigh)
    if numneigh GT 0 then $
	    avdiff=avg(diff[i,neighbors]) $
    else    avdiff=0.
    subtweak[i]=avdiff*softening
  endfor

;check for convergence, apply subtweaks, and bookkeeping

;this is probably not the ideal convergence criterion.
;most likely want something related to the niose in the differences
;between images....
  if iter GT 0 then converged=min(abs(subtweak) LT tol) else converged=0

  if not converged then begin
    for i=0,nim-1 do begin
       for j=0,nim-1 do begin
          diff[i,j]=diff[i,j]-subtweak[i]+subtweak[j]
       endfor
    endfor

    totaltweak=totaltweak+subtweak
  endif

  print,'iter: ',iter
  print,subtweak

  if iter GT maxiter then begin
	  converged=1
	  print,'Max its reached.'
  endif
  iter=iter+1
endwhile

tweak=totaltweak
print,'total:'
print,tweak

return,tweak
end
