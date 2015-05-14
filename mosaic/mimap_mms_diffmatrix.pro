;+
; NAME:
;   MIMAP_MMS_DIFFMATRIX
;
; VERSION:
;   0.0 (Mar, 2008)
;
; PURPOSE:
;   Get the matrix of flux differences in the overlapping regions of a set
;   of images
;
; REFERENCE:
;   Smiley, G. 2008 ApJ
;
; CATEGORY:
;   Image Manipulation
;
; CALLING SEQUENCE:
;   diff=mimap_mms_diffmatrix(im,nover=nover,mask=mask)
;
; INPUTS:
;    im - an Nx x Ny x Nim array of images, matched pixel by
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
FUNCTION mimap_mms_diffmatrix,im,nover=nover,mask=mask,debug=debug

minover=1E3
sz=size(im)
nim=sz[3]
nover=fltarr(nim,nim)
diff=fltarr(nim,nim)
if arg_present(mask) EQ 0 then mask=intarr(sz[1:3])+1

for i=0,nim-1 do begin
  for j=i+1,nim-1 do begin
    if j NE i then begin

      im1=im[*,*,i] & im2 = im[*,*,j]
      g=where(finite(im1) and finite(im2) $
              and (im1 NE 0) and (im2 NE 0) and mask[*,*,i] and mask[*,*,j], $
              count)
      nover[i,j]=count & nover[j,i]=count

      if count GT minover then begin
         dd=im1[g]-im2[g]

         oo=sort(dd)
         pn=round(count/[16.,84.])
         binsz=stddev(dd[oo[pn[1]:pn[0]]])

         diff[i,j]=mode(dd,binsz);3*median(dd)-2*avg(dd)
         if keyword_set(debug) then begin
		plothist,dd,bin=binsz
         	oplot,fltarr(2)+diff[i,j],[0,1E7],linestyle=2
	 endif
         tmp=''
         ;sn[i,j]=diff[i,j]/binsz
         if keyword_set(debug) then begin
            print,i,j,diff[i,j],binsz
            read,tmp
          endif
         diff[j,i]=0.-diff[i,j]
      endif

    endif
 endfor
endfor

return,diff


end
