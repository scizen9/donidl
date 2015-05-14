;+
; NAME:
;   MIMAP_MMS_DIFFSKY_SLOW
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   Get the matrix of flux differences in the overlapping regions of a set
;   of images.  use when there are *many* large images, otherwise use
;   mimap_mms_diffmatrix 
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   diff=mimap_mms_diffsky_slow(im,nover=nover,mask=mask)
;
; INPUTS:
;    im - an Nim array of images names, matched pixel by
;         pixel. Use MASK to define regions of the image that should
;         not be considered in the overlap
;
; OPTIONAL INPUTS:
;    nover - if supplied, returns to the user a matrix of the number
;            of overlapping pixels between the supplied images
;
; OUTPUT:
;    diff - An Nim x Nim array of differences between each pair of images.
;          The sign is such that diff[i,j]=im[i]-im[j]
;
; COMMENTS:
;    Differrences are hardwired to be calculated as a pseudo-mode:
;    3*median-2*mean (Kenney 1962).  this should probably be allowed
;    as an option.  Update:  pseudo-mode sucks, especially when
;    there's stray light.  use the real mode.  how to determine
;    binsize? could also do it by fitting gaussians to the meat of the
;    distribution. 
;
; REVISION HISTORY:
;    Oct 2008 - written, B. Johnson
;
;--------------------------------------------------------------------
FUNCTION mimap_mms_diffsky_slow,diff,nover,im,bad, $
	pixratio=pixratio,large=large
;
; check inputs
nim=n_elements(im)
;
; warn, if we are large
if keyword_set(large) then $
	print,'Large image set, this may take a while.' $
else	ims = mimap_mms_loadims(im)	; else pre-load ims
;
; if no baddies input, redo them all
if n_params(0) lt 4 then begin
	bad = lindgen(nim)
	plab = 'Calculating diffs...'
	diff = fltarr(nim,nim)	; reset diff matrix
endif else plab = 'Fixing baddie diffs...'
nbad=n_elements(bad)
;
; minimum number of overlap pixels
minover=1E3
;
; pixratio
if keyword_set(pixratio) then $
	pxrat=pixratio $
else	pxrat=1.0

print,plab
; loop over each bad image
for b=0,nbad-1 do begin
  i = bad[b]
  ; get initial img
  if keyword_set(large) then $
  	im1=mrdfits(im[i],/silent) $
  else	im1=ims[*,*,i]
  ; calculate diffs for each of the rest of the images
  for j=0,nim-1 do begin

    ; check only unique images
    if j NE i then begin

      ; comparison image
      if keyword_set(large) then $
      		im2 = mrdfits(im[j],/silent) $
      else	im2 = ims[*,*,j]
      ; good pixels
      g=where(finite(im1) and finite(im2) and (im1 NE 0) and (im2 NE 0),count)
      nover[i,j]=count 
      nover[j,i]=count

      ; will we get good statistics?
      if count GT minover then begin

	; calculate modal difference
        ;dd=im1[g]-im2[g]
        ;oo=sort(dd)
        ;pn=round(count/[16.,84.])
        ;binsz=stddev(dd[oo[pn[1]:pn[0]]])
        ;diff[i,j]=mode(dd,binsz)/pxrat
	sky,im1[g],sk1,ss1,/nan,/silent
	sky,im2[g],sk2,ss2,/nan,/silent
	diff[i,j] = (sk1-sk2)/pxrat
	; complementary diffs
        diff[j,i]=0.-diff[i,j]

      endif	; good statistics?

    endif	; unique and overlapping?

  endfor	; loop over rest of images
  print,i+1,'/',nim,' diffs for ',im[i],format='(i4,a1,i4,a,a)'
endfor		; loop over all images

return,diff

end
