pro wise_sort_fits,unlimited=unlimited,atlas=atlas,frames=frames
;+
; wise_sort_fits - put images in specific host named directories
;-
; directories
workdir=!GLGA_WISE_DATA+'work/'
sortdir=!GLGA_WISE_DATA+'data/sort/'
if keyword_set(atlas) then begin
	datadir=!GLGA_WISE_DATA+'data/L3a/'
	fspec = '*/*-w1-int-3.fits'
	imsize = 4095
	scrfil = 'cp_script_atlas_sort'
endif else begin
	datadir=!GLGA_WISE_DATA+'data/L1b/'
	fspec = '*a/???/*-w1-int-1b.fits'
	imsize = 1016
	scrfil = 'cp_script_frames_sort'
endelse
;
; object list
fn=workdir+'objmultipos.list'
if not file_test(fn,/read,/regular) then begin
	print,'WISE_SORT_FITS: Error - objmultipos.list not found: ',fn
	return
endif
readcol,fn,idx,pra,pdec,name,f='F,F,F,A'
name=strcompress(name,/remove_all)
np=n_elements(pra)
;
; image list
im=file_search(datadir+fspec,/expand_tilde)
nim=n_elements(im)
;
; check: are we in the image?
stat=fltarr(nim,np)
print,'Checking images...'

for iim=0,nim-1 do begin

   print,string(13B),iim+1,'/',nim,im[iim],format='($,a1,i7,a1,i7,2x,a)'
   hdr=headfits(im[iim])
   adxy,hdr,pra,pdec,px,py
   inim=where(px GE 0 and px LE imsize and py GE 0 and py LE imsize,count)
   if count GE 1 then stat[iim,inim]=1

endfor
print,' '
print,'Done.'
;
; get status and resulting coverage
nc=total(stat,1)
cov=where(nc GT 0,numcov)
;
; file lists
im=repstr(im,'w1','w?')
un=repstr(im,'-int-','-unc-')
cv=repstr(im,'-int-','-cov-')
mk=repstr(im,'-int-','-msk-')
;
; script file
filestamp,scrfil
openw,sl,scrfil,/get_lun
print,'Writing script file...'

for i=0,numcov-1 do begin

   inc=where(stat[*,cov[i]] GT 0,nin)
   print,string(13B),i+1,'/',numcov,name[cov[i]],format='($,a1,i7,a1,i7,2x,a-25)'
   if file_test(sortdir+name[cov[i]]) EQ 0 then begin
      spawn,'mkdir '+sortdir+name[cov[i]]
   endif
   if keyword_set(unlimited) then $
	nlim = nin $
   else	nlim = 9
   for j=0,(nin-1)<nlim do begin	; more than nlim is too many

      printf,sl,'cp '+im[inc[j]]+' '+sortdir+name[cov[i]]+'/'
      printf,sl,'cp '+cv[inc[j]]+' '+sortdir+name[cov[i]]+'/'
      printf,sl,'cp '+un[inc[j]]+' '+sortdir+name[cov[i]]+'/'
      if keyword_set(frames) then $
      	printf,sl,'cp '+mk[inc[j]]+' '+sortdir+name[cov[i]]+'/'

   endfor

endfor
free_lun,sl
print,' '
print,'Done.'

return
end
