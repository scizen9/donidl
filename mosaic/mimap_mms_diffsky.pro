;+
; NAME:
;   MIMAP_MMS_DIFFSKY
;
; VERSION:
;   0.0 (Sep, 2011)
;
; PURPOSE:
;   Get the matrix of flux differences in the overlapping regions of a set
;   of images.
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   diff=mimap_mms_diffsky(im,nover=nover)
;
; INPUTS:
;    im - an Nim array of images names, matched pixel by
;         pixel. Use MASK to define regions of the image that should
;         not be considered in the overlap
;
; OPTIONAL INPUTS:
;    nover - if supplied, returns to the user a matrix of the number
;            of overlapping images (should be 0 along the diagnonal)
;
; OUTPUT:
;    diff - An Nim x Nim array of differences between each pair of images.
;          The sign is such that diff[i,j]=im[i]-im[j]
;
; COMMENTS:
;    Use the sky routine which calls MMM
;
; REVISION HISTORY:
;    Sep 2011 - written, D. Neill
;
;--------------------------------------------------------------------
FUNCTION mimap_mms_diffsky,im,nover=nover,bad=bad

nim=n_elements(im)
nover=fltarr(nim,nim)
diff=fltarr(nim,nim)
skys=fltarr(nim)
skysigs=fltarr(nim)

; get sky for each image
for i=0,nim-1 do begin
	im1=mrdfits(im[i],/silent)
	sky,im1,skymode,skysig,/nan,/silent
	skys[i] = skymode
	skysigs[i] = skysig
	print,i+1,'/',nim,' sky for ',im[i],' : ',skymode,' +- ',skysig, $
		format='(i4,a1,i4,a,a,a,f10.7,a,f10.7)'
endfor
; check for baddies
bad = [-1]
if nim gt 2 then begin
	ims,skysigs,ms,sg,wgt,siglim=2.0
	print,'Avg skysig: ',ms,' +- ',sg,form='(a,f10.7,a,f10.7)'
	bad=where(wgt lt 1, nbad)
	if nbad gt 0 then begin
		print,'Baddies:'
		bads = indgen(nim)
		bads = bads[bad]
		forprint,bads,im[bad],textout=2,form='(i3,2x,a)'
	endif
endif
; fill diffs matrix 
for i=0,nim-1 do begin
	; calculate diffs for each of the rest of the images
	for j=i+1,nim-1 do begin

		nover[i,j]=1
		nover[j,i]=1
		diff[i,j] = skys[i] - skys[j]
		; complementary diffs
		diff[j,i]=0.-diff[i,j]

	endfor	; loop over rest of images
endfor		; loop over all images

return,diff

end
