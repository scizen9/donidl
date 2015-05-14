pro sdss_sort_fits,unlimited=unlimited
;+
; sdss_sort_fits - put images in specific host named directories
;-
; directories
datadir=!GLGA_SDSS_DATA+'data/raw/'
sortdir=!GLGA_SDSS_DATA+'data/sort/'
;
; object list
fn='objmultipos.list'
if not file_test(fn,/read,/regular) then begin
	print,'SDSS_SORT_FITS: Error - objmultipos.list not found: ',fn
	return
endif
readcol,fn,idx,pra,pdec,name,f='F,F,F,A'
name=strcompress(name,/remove_all)
np=n_elements(pra)
;
; image list
im=file_search(datadir+'fpC*u*gz',/expand_tilde)
nim=n_elements(im)
;
; check: are we in the image?
stat=fltarr(nim,np)
print,'Checking images...'

for iim=0,nim-1 do begin

   print,string(13B),iim+1,'/',nim,im[iim],format='($,a1,i7,a1,i7,2x,a)'
   hdr=headfits(im[iim])
   adxy,hdr,pra,pdec,px,py
   inim=where(px GE 0 and px LE 2048 and py GE 0 and py LE 1489,count)
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
im=repstr(im,'u','*')
ts=repstr(im,'fpC','tsField')
ts=repstr(ts,'.gz','')
ts=repstr(ts,'*6','6-*')
ts=repstr(ts,'*1','1-*')
ts=repstr(ts,'*2','2-*')
ts=repstr(ts,'*3','3-*')
ts=repstr(ts,'*4','4-*')
ts=repstr(ts,'*5','5-*')
dr=repstr(ts,'tsField','drField')
;
; script file
scrfil = 'cp_script_fits_sort'
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
      printf,sl,'cp '+ts[inc[j]]+' '+sortdir+name[cov[i]]+'/'
      printf,sl,'cp '+dr[inc[j]]+' '+sortdir+name[cov[i]]+'/'

   endfor

endfor
free_lun,sl
print,' '
print,'Done.'

return
end
