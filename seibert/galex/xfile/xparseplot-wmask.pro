pro xparseplot, xparsefile, binsize=binsize,$ 
                adaptivebin=adaptivebin,snr=snr,ps=ps

; read outpout from xfileparse and plot
;
; xparsefile = name of sav file from xfileparse
;     contains: 
;     t,ra,dec -> arrays from extended photon file 
;     ra0,dec0 -> position of source
;     rr -> relative repsonse at ra0,dec0
;     diam -> diameter of circular aperture
;     expstart,expend,exptime -> from rr header converted to s/c time
;     obsdate -> string from rr header
;     field -> tilename+visit
;     clock_time ->  s/c time intervals of 0.1 seconds over exposure
;     clock -> clock=1 if any (not just source) photons exist in interval
;              clock=0 otherwise
;
; binsize = fixed bin size in seconds (defaults to 60s)
; adaptivebin = use adaptive binning (based upon counting snr)
;               if selected binsize is ignored
; snr = snr to use if adaptive binning is set
; ps = output to postscript if set (defaults to screen)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;read sav file and set up


ext=strpos(xparsefile,'.sav')
file=strmid(xparsefile,0,ext)

if not keyword_set(binsize) then binsize=60

restore, xparsefile

n=strpos(xparsefile,'-nd-')
f=strpos(xparsefile,'-fd-')
if n[0] ge 0 then band='n'
if f[0] ge 0 then band='f'
if band eq 'f' then zp=18.82
if band eq 'n' then zp=20.08

clockinterval=0.1 ; fixed in xfileparse


;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;fixed time intervals

if not keyword_set(adaptivebin) then begin

psfile=file+'-fixed'+strcompress(string(binsize),/rem)+'.ps'
datfile=file+'-fixed'+strcompress(string(binsize),/rem)+'.dat'

interval=binsize
bins=ceil((expend-expstart)/interval)
time=fltarr(bins)
photons=fltarr(bins)
mag=fltarr(bins)
magerr=fltarr(bins)

for i=0,bins-1 do begin

 time[i]=(i+1)*interval
 mag[i]=!values.f_nan
 magerr[i]=!values.f_nan

 a=where(t ge expstart+interval*i and t lt expstart+interval*(i+1) $
         and (flags and 4) eq 0,count)
 masked=where(t ge expstart+interval*i and t lt expstart+interval*(i+1) $
         and (flags and 64) ne 0,count_masked)

 if count gt 0 and count_masked/count lt 0.1 then begin
  photons[i]=count
  aa=where(clock_time ge expstart+interval*i and $
           clock_time lt expstart+interval*(i+1) and $
           clock gt 0, bcount)
  fracdt=bcount*clockinterval
  rrfrac=rr*fracdt/exptime
  ;rrfrac=rr*interval/exptime
  mag[i]=-2.5*alog10(photons[i]/rrfrac)+zp
  magerr[i]= (2.5/alog(10)) * (sqrt(photons[i])/rrfrac)/(photons[i]/rrfrac)
 endif

endfor 

endif 


;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;adaptive time intervals

if keyword_set(adaptivebin) then begin
if not keyword_set(snr) then snr=5

psfile=file+'-adapt'+strcompress(string(snr),/rem)+'.ps'
datfile=file+'-fixed'+strcompress(string(binsize),/rem)+'.dat'

dt=clockinterval
tt=dt
delvarx, photons,time,mag,magerr

while tt lt exptime>(max(t)-expstart) do begin
 m=!values.f_nan
 me=!values.f_nan
 a=where(t ge expstart+tt-dt and t lt expstart+tt $
         and (flags and 4) eq 0,count) 
 if count gt 0 and  count/sqrt(count) gt snr then begin
  aa=where(clock_time ge expstart+tt-dt and $
           clock_time lt expstart+tt and $
           clock gt 0, bcount)
  fracdt=bcount*clockinterval
  ;print, dt, fracdt,bcount
  rrfrac=rr*fracdt/exptime
  m=-2.5*alog10(count/rrfrac)+zp
  me= (2.5/alog(10)) * (sqrt(count)/rrfrac)/(count/rrfrac)
  if n_elements(photons) gt 0 then photons=[photons,count] else photons=count
  if n_elements(mag) gt 0 then mag=[mag,m] else mag=m
  if n_elements(magerr) gt 0 then magerr=[magerr,me] else magerr=me 
  if n_elements(time) gt 0 then time=[time,tt] else time=tt 
  dt=clockinterval
 endif else begin
  dt=dt+clockinterval
 endelse
 tt=tt+clockinterval
endwhile

endif 


;;;;;;;;;;;;;;;;;;;;; 
;;;;;;;;;;;;;;;;;;;;;
;total integrated mag

;intmag= -2.5*alog10(n_elements(t)/rr)+zp

bidx=where((flags and 4) ne 0)
gidx=where((flags and 4) eq 0)
tb=t[bidx]
tg=t[gidx]
intmag= -2.5*alog10(n_elements(tg)/rr)+zp


;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;make the plot

if keyword_set(ps) then $
 ps_start,filename=psfile,xsize=6, ysize=8.5,/inches,$
 xoffset=1.25,yoffset=1.5,charsize=0.8, /color $
else window, 0, xsize=500, ysize=750

setplotcolors

if band eq 'n' then filter='NUV'
if band eq 'f' then filter='FUV'

if keyword_set(adaptivebin) then $
 bs='adaptive w/snr='+strcompress(string(snr),/rem) else $
 bs=strcompress(string(binsize,format='(f7.2)'),/rem)+'s'

plot,time,mag,psym=-sym(1),xr=[min(time)<0,max(time)+20],/xs,$
 yr=[max(mag)+0.5,min(mag)-0.5],/ys,$
 xtit='Time (seconds, bin='+bs+')',$
 ytit=filter+' Mag (not bg subtracted)',symsize=.6,$
 position=[.12,.65,.95,.95],/norm;,/nodata

oploterr,time,mag,magerr,sym(0)

gtime=fltarr(n_elements(tg))+intmag
oplot,tg-expstart,gtime,psym=3,color=!red

fit=linfit(tg-expstart,ra[gidx])
x=indgen((exptime>(max(t)-expstart))*1.5)
y=fit[0]+x*fit[1]
plot,tg-expstart,ra[gidx],psym=3,/ynozero,/noerase,$
 xtit='t (m='+string(fit[1])+')',ytit='RA',$
 xr=[0,max(t)-expstart],/xs,$
 xtickinterval=500,position=[.13,.40,.45,.58],/norm
oplot,tb-expstart,ra[bidx],psym=3,color=!red
oplot,x,y

fit=linfit(tg-expstart,dec[gidx])
y=fit[0]+x*fit[1]
plot,tg-expstart,dec[gidx],psym=3,/ynozero,/noerase,$
 xtit='t (m='+string(fit[1])+')',ytit='DEC',$
 xr=[0,max(t)-expstart],/xs,$
 xtickinterval=500,position=[.63,.40,.95,.58],/norm
oplot,tb-expstart,dec[bidx],psym=3,color=!red
oplot,x,y


phalfwidth=25 ; arcseconds

plot,ra[gidx],dec[gidx],psym=3,/xs,/ys,$
 xr=[ra0+phalfwidth/3600./cos(dec0),ra0-phalfwidth/3600./cos(dec0)],$
 yr=[dec0-phalfwidth/3600.,dec0+phalfwidth/3600.],$
 xtit='RA', ytit='DEC',xticks=2,$
 position=[.13,.05,.45,.34],/norm,/noerase


xyouts,0.5,0.29,'Fld: '+field,/norm

xyouts,0.5,0.26,'RA, DEC: '+string(ra0,format='(f10.5)')+', '+$
       string(dec0,format='(f10.5)'),/norm

tstart= expstart + (315532800+432000) ; convert (back) to unix time
xyouts,0.5,0.23,'UTC Start: '+systime(0, tstart,/utc),/norm

tend= expend + (315532800+432000) ; convert (back) to unix time
xyouts,0.5,0.20,'UTC  End: '+systime(0, tend,/utc),/norm

xyouts,0.5,0.17,'Exptime: '+string(exptime,format='(f8.2)'),/norm

xyouts,0.5,0.14,'Rel. Resp.: '+string(rr,format='(f8.2)'),/norm

xyouts,0.5,0.11,'Ap. Diam.("): '+string(diam,format='(f5.1)'),/norm

xyouts,0.5,0.08,'Int. Mag: '+string(intmag,format='(f7.3)'),/norm

if keyword_set(ps) then ps_end


;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;
;write the binned data

openw,lun,datfile,/get_lun
!textunit=lun
printf,lun,'# Field       : '+field
printf,lun,'# Band        : '+filter
printf,lun,'# RA, DEC     : '+string(ra0,format='(f10.5)')+', '+$
                          string(dec0,format='(f10.5)')
printf,lun,'# UTC Start   : '+systime(0, tstart,/utc)
printf,lun,'# UTC End     : '+systime(0, tend,/utc)
printf,lun,'# Exptime     : '+string(exptime,format='(f8.2)')
printf,lun,'# Rel. Resp.  : '+string(rr,format='(f8.2)')
printf,lun,'# Ap. Diam.("): '+string(diam,format='(f5.1)')
printf,lun,'# Int. Mag    : '+string(intmag,format='(f7.3)')
printf,lun,'# unix_time, elapsed_time, mag, magerr, counts'

forprint,expstart+(315532800+432000)+time,time,mag,magerr,photons,$
 /nocomment,textout=5,format='e18.11,f10.3,f8.3,f7.3,i'

free_lun,lun

end



