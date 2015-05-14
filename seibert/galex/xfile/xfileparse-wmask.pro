pro xfileparse, band=band, field=field, xpath=xpath, $
       ra=ra, dec=dec, diam=diam, dpath=dpath, outpath=outpath,$
       clean=clean

;-------------------------------------       

if not keyword_set(band) or not keyword_set(field) or $
   not keyword_set(dpath)  then begin
 print, 'Must have field, band, and direct image data path'
 goto, errorjump
endif

if not keyword_set(xpath) then xpath='./'
if not keyword_set(outpath) then outpath='./'

if not keyword_set(diam) then diam=24 ; circular aperture radius in arcseconds
rad=ceil(diam/2.)


xfile=field+'-'+band+'d-x.fits'
if not keyword_set(xpath) then xpath=dpath

;;;;;;;;;;;;;;;;;;;;;;;
;build x file if needed

buildxfile=0
if not file_exist(xpath+xfile) then begin
 buildxfile=1
 rawfile=field+'-'+band+'d-raw6.fits'
 pipepath='/home/galex/ops/bin/'
 cmd=pipepath+'rta -ip '+dpath+' -phf '+rawfile+' -userta -cali cal2path='+ $
     dpath+'/cal -op '+outpath

 openw,lun,'xfilecreate.s',/get_lun
 printf,lun,pipepath+'newcfg ops -exec \ '
 printf,lun, cmd
 printf,lun, 'exit'
 free_lun,lun
 spawn,'chmod +x xfilecreate.s'
 spawn,'./xfilecreate.s'
 xpath=outpath
endif

xhdr1=headfits(xpath+xfile,exten=1)
n=sxpar(xhdr1,'NAXIS2')


if band eq 'f' then zp=18.82
if band eq 'n' then zp=20.08


rrfile=field+'-'+band+'d-rr.fits*'  
mcatfile=field+'-xd-mcat.fits*'

if not keyword_set(ra) and not keyword_set(dec) then begin
 m=mrdfits(dpath+mcatfile,1)
 select=where(m.fuv_mag_aper_5+zp gt 0 and $
             m.fuv_mag_aper_5+zp lt 18.5 and $
             m.fov_radius le 0.55 and m.nuv_flags eq 0)
 m=m[select]
endif else begin
 m=create_struct('alpha_j2000',ra[0],'delta_j2000',dec[0])
 m = make_array(val = m, dim = n_elements(ra))
 m.alpha_j2000 = ra
 m.delta_j2000 = dec
endelse


for i=0,n_elements(m)-1 do begin
 result=execute('delvarx, s'+strcompress(string(i),/rem)+'_t')
 result=execute('delvarx, s'+strcompress(string(i),/rem)+'_ra')
 result=execute('delvarx, s'+strcompress(string(i),/rem)+'_dec')
endfor


chunk=50000 ; number of rows to read in at a time
if band eq 'n' then chunk =100000
rows=lindgen(chunk) ; define row list
done=0 ; flag for eof
cooscale=2147483648.0 ; coordiante system scale factor (2^31)


;extract need info from rr
rrmap=mrdfits(dpath+'/'+rrfile,0,rrhdr)
obsdate=sxpar(rrhdr,'OBS-DATE')
exptime=sxpar(rrhdr,'EXPTIME')
expstart=sxpar(rrhdr,'EXPSTART')
expend=sxpar(rrhdr,'EXPEND')
expstart=expstart - (315532800+432000) ;convert to s/c time
expend=expend - (315532800+432000) ;convert to s/c time
extast,rrhdr,astr


;establish clock 
interval=0.1 ;minimal resolution
clock=intarr(ceil((expend-expstart)/interval))
clock_time=dblarr(n_elements(clock))
for ii=0, n_elements(clock)-1 do clock_time[ii]=expstart+(ii+1)*interval


print, ' '
print, 'parsing '+xfile

tstart=systime(1)

for i=0,(ceil(1.*n/chunk))-1 do begin

  print,' chunk '+strcompress(string(i+1),/rem)+' of '+$
         strcompress(string( ceil(1.*n/chunk) ),/rem)

  rowlimit=where(rows le n-1,rowlimit_n)
  if rowlimit_n gt 0 then rows=rows[min(rowlimit):max(rowlimit)] else done=1

  if not done then begin 

    ftab_ext,xpath+xfile,[1,6,7,8,9],t,flags,x,y,z,rows=rows,exten=1

    ;filter out bad photons 
    ; skip out if nothing but bad photons
    a=where((flags and 4) eq 0, counta)
    if counta eq 0 then goto, nextchunck    
    ;t=t[a]
    ;flags=flags[a] 
    ;x=x[a]
    ;y=y[a]
    ;z=z[a]

    ;build clock
    trange=minmax(t)
    lo=value_to_index(clock_time,trange[0])
    hi=value_to_index(clock_time,trange[1])
    for ii=(lo-1)>0, (hi+1)<(n_elements(clock_time)-1) do begin
     if clock[ii] eq 0 then begin
       a=where(t ge expstart+ii*interval and $
               t lt expstart+(ii+1)*interval and (flags and 4) eq 0,count)
       if count gt 0 then clock[ii]=1
     endif
    endfor
    

    ; compute ra & dec
    ; make sure z is between -1 and 1
    z=z/cooscale
    l=where(z gt 1,count_l)
    if count_l gt 0 then z[l]=1.
    l=where(z lt -1,count_l)
    if count_l gt 0 then z[l]=-1.
    d=180/!pi*ASIN(z)
    r=180/!pi*atan(y/cooscale,x/cooscale)
    wrap=where(r lt 0, countwrap)
    if countwrap gt 0 then r[wrap]=r[wrap]+360


    ; loop through selected objects
    for j=0,n_elements(m)-1 do begin
    ;for j=0,1 do begin

     srcn='s'+strcompress(string(j),/rem)+'_'

     ; find target photons within circular aperture
     ; tt= target time, tra = target ra, tdec = target dec
     gcirc,1,m[j].alpha_j2000/15,m[j].delta_j2000,r/15,d,dis
     ap=where(dis le rad, count_ap)
;     ap=where(dis le rad and (flags and 4) eq 0,count_ap)
;     ap=where(dis le rad and ((flags and 4) eq 0 or (flags and 64) ne 0),$
;               count_ap)
     if count_ap gt 0 then begin
       result=execute('varexist=n_elements('+srcn+'t)')
       if varexist eq 0 then begin 
          result=execute(srcn+'ra=r[ap]')
          result=execute(srcn+'dec=d[ap]')
          result=execute(srcn+'t=t[ap]')
          result=execute(srcn+'flags=flags[ap]')
       endif else begin 
          result=execute(srcn+'ra=['+srcn+'ra,r[ap]]')
          result=execute(srcn+'dec=['+srcn+'dec,d[ap]]')
          result=execute(srcn+'t=['+srcn+'t,t[ap]]')
          result=execute(srcn+'flags=['+srcn+'flags,flags[ap]]')
       endelse
     endif

    endfor 

    nextchunck:

    ; update row list for next chunk
    rows=rows+chunk

   endif ; done (eof)

   delvarx, t,flags,x,y,z,d,r
   delvarx, dis,ap,wrap

endfor

delvarx, rowlimit,rows

print, xfile+' parsed in '+strcompress(string(systime(1) - tstart,format='(f7.2)'),/rem), ' seconds' 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;save extracted data to outpath


spawn,'ls -d '+outpath,result,err
if result eq '' then spawn, 'mkdir '+outpath

for j=0,n_elements(m)-1 do begin

 srcn='s'+strcompress(string(j),/rem)+'_'

 result=execute('ra='+srcn+'ra')
 result=execute('dec='+srcn+'dec')
 result=execute('t='+srcn+'t')
 result=execute('flags='+srcn+'flags')

 ra0=m[j].alpha_j2000
 dec0=m[j].delta_j2000

 ad2xy,ra0,dec0,astr,x,y
 rr=rrmap[x,y]

 if dec0 ge 0 then sign='+' else sign=''

 ofile=outpath+'/'+field+'_'+$
    strcompress(string(ra0),/rem)+$
    sign+strcompress(string(dec0),/rem)+$
    '-'+band+'d-x.sav'

 save,filename=ofile,t,ra,dec,flags,rr,$
      expstart,expend,exptime,obsdate,$
      ra0,dec0,diam,field,clock,clock_time

 result=execute('delvarx,'+srcn+'ra')
 result=execute('delvarx,'+srcn+'dec')
 result=execute('delvarx,'+srcn+'t')
 delvarx,ra,dec,t,rr

 print,'wrote file '+ofile

endfor


if buildxfile and keyword_set(clean) then begin

spawn, 'rm '+outpath+'/'+xfile,result
print, 'deleted extended photon file: '+xpath+'/'+xfile

endif

errorjump:
print, 'finis'

end




