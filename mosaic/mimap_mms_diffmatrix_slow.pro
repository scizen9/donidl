;+
; NAME:
;   MIMAP_MMS_DIFFMATRIX_SLOW
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
;   diff=mimap_mms_diffmatrix_slow(im,nover=nover,mask=mask)
;
; INPUTS:
;    im - an Nim array of images names, matched pixel by
;         pixel. Use MASK to define regions of the image that should
;         not be considered in the overlap
;
; OPTIONAL INPUTS:
;    nover - if supplied, returns to the user a matrix of the number
;            of overlapping pixels between the supplied images
;    mask - an array of same dimensions as IM that has zero for pixels
;           which should not be considered in the overlap, and 1 for
;           pixels that should be considered.  If not supplied, only
;           non-zero finite pixels will be considered.
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
FUNCTION mimap_mms_diffmatrix_slow,im,bad,nover=nover,overlap=overlap

minover=1E3

nim=n_elements(im)
nover=fltarr(nim,nim)
diff=fltarr(nim,nim)

if keyword_set(overlap) then $
   if total(overlap) EQ 0 then overlap=fltarr(nim,nim)+1
if keyword_set(overlap) EQ 0 then overlap=fltarr(nim,nim)+1

; loop over each image
for i=0,nim-1 do begin

  im1=mrdfits(im[i],/silent)
  ; calculate diffs for each of the rest of the images
  for j=i+1,nim-1 do begin

    ; check only unique overlapping images
    if j NE i and overlap[i,j] then begin

      ; comparison image
      im2 = mrdfits(im[j],/silent)
      ; good pixels
      g=where(finite(im1) and finite(im2) and (im1 NE 0) and (im2 NE 0),count)
      nover[i,j]=count 
      nover[j,i]=count

      ; will we get good statistics?
      if count GT minover then begin

	; calculate modal difference
        dd=im1[g]-im2[g]
        oo=sort(dd)
        pn=round(count/[16.,84.])
        binsz=stddev(dd[oo[pn[1]:pn[0]]])
        diff[i,j]=mode(dd,binsz)
	; complementary diffs
        diff[j,i]=0.-diff[i,j]

      endif	; good statistics?

    endif	; unique and overlapping?

  endfor	; loop over rest of images
  print,i+1,'/',nim,' diffs for ',im[i],format='(i4,a1,i4,a,a)'
endfor		; loop over all images

overlap=(nover GT minover)

return,diff

end
