pro gx_imstat, id, ra, dec, majordiam, $
summarylog=summarylog,$
missionstatusfile=missionstatusfile,$
gi_age_min=gi_age_min, $
extra=extra
;+
; gx_imstat - get GALEX imaging status for positions and sizes
;
; Usage: gx_imstat,id,ra,dec,majordiam,summarylog=summarylog, $
;	missionstatusfile=missionstatusfile,gi_min_age=gi_min_age
;
; INPUTS:
; ID	- ids of objects
; RA,DEC- coords in decimal degrees (j2000)
; MAJORDIAM - diameter of objects in arcminutes
;
; KEYWORDS:
; SUMMARYLOG - output file name
; MISSIONSTATUSFILE - GALEX mission status fits file
; GI_AGE_MIN - minimum allowed age of GI data
; EXTRA - print extra information for each image file and data copy script
;	in cp_data.csh
;
;-
;;;;;;;;;
;defaults

if not keyword_set(missionstatusfile) then $
missionstatusfile='/home/galex/fltops/logs/mission_status/mission_status.fits'
; missionstatusfile=!GLGA_ROOT+'work/mission_status_sg.fits'

if not keyword_set(gi_age_min) then $
 gi_age_min = 8*30 ; GI data should be 8 months old to include

;;;;;;;;;;;;;;;;;;;;;;;;
; open log files

filestamp,summarylog
openw,ol, summarylog, /get_lun

;;;;;;;;;;;;;;;;;;;;;;;;;
;read mission status file

hdr=headfits(missionstatusfile,ext = 0)
msdate = sxpar(hdr, 'RUNDAT')
g=mrdfits(missionstatusfile,1, /silent)
g.nuv_exptime = g.nuv_exptime > 0
g.fuv_exptime = g.fuv_exptime > 0
g.surv_type = strcompress(g.surv_type,/rem)
g.grelease = strcompress(g.grelease,/rem)
g.qa_grade = strcompress(g.qa_grade,/rem)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;filter on usable image data

a = where( (g.nuv_exptime gt 0 or g.fuv_exptime gt 0) and $
           g.surv_type ne 'SIO' and g.surv_type ne 'SSO' and $
           g.surv_type ne 'SOO' and g.ow eq 'd' and g.qa_grade ne 'FAIL' and $
           g.asp_ave_ra_rta ne -9999. and g.asp_ave_dec_rta ne -9999.,count)

g = g[a]

;filter out GI data less thean age minimum

year='20'+strmid(g.ecl_start,0,2)
month=strmid(g.ecl_start,2,2)
day=strmid(g.ecl_start,4,2)
obsdate=julday(month,day,year)
curdate=systime(/julian)
elapsed_days = curdate-obsdate

a = where(g.surv_type eq 'GII' and elapsed_days lt gi_age_min, complement=b)
g=g[b]

;if no qa_grade change to NA
a = where(g.qa_grade eq '')
g[a].qa_grade =  'NA'

ngood=n_elements(ra)

if keyword_set(extra) then begin
	sfile='cp_data.csh'
	filestamp,sfile,/arch
	openw,cl,sfile,/get_lun
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;loop through the batch from input catalog

for i = 0, ngood-1 do begin

 gcirc, 1, ra[i]/15., dec[i], g.asp_ave_ra_rta/15., g.asp_ave_dec_rta,  dist

 ; check for good overlap first
 
 a = where(dist/60. le 36 and $
           (g.fuv_exptime gt 10 or g.nuv_exptime gt 10),  count)

 ; now get all fuv and nuv in extended region

 f = where(dist/60. le 35 + majordiam[i] and $
            g.fuv_exptime gt 10,  countfuv)
 n = where(dist/60. le 35 + majordiam[i] and $
            g.nuv_exptime gt 10,  countnuv)

 ; if there is nothing within 36 arcmin then covereage is too poor
 if count eq 0 then begin
  countfuv=0
  countnuv=0 
  fuvexp=0.
  nuvexp=0.
 endif else begin
  if countfuv gt 0 then $
	fuvexp = total(g[f].fuv_exptime) $
  else	fuvexp = 0.
  if countnuv gt 0 then $
	nuvexp = total(g[n].nuv_exptime) $
  else	nuvexp = 0.
 endelse

 print,string(13B),i+1,'/',ngood,ra[i],countfuv,countnuv,id[i], $
	 format='($,a1,i6,a,i6,f15.8,2i5,2x,a-25)'

 printf,ol,id[i],ra[i],dec[i],majordiam[i],countfuv,countnuv,fuvexp,nuvexp, $
	 format='(a-25,2f13.8,f9.2,2i5,2f11.2)'
 if keyword_set(extra) then begin
	 printf,cl,'mkdir '+id[i]
	 printf,cl,'cd '+id[i]
	 for j=0,countfuv-1 do printf,ol,j+1,'FUV',dist[f[j]]/60., $
		 g[f[j]].fuv_exptime,strtrim(g[f[j]].info_str,2)+'/'+ $
		 strtrim(g[f[j]].base,2), form='(i7,a5,2x,f9.3,f10.1,2x,a)'
	 for j=0,countfuv-1 do printf,cl,'cp '+strtrim(g[f[j]].info_str,2)+ $
		 '/'+strtrim(g[f[j]].base,2)+'-fd-{int,cnt,rrhr}.fit* .'
	 for j=0,countnuv-1 do printf,ol,j+1,'NUV',dist[n[j]]/60., $
		 g[n[j]].nuv_exptime,strtrim(g[n[j]].info_str,2)+'/'+ $
		 strtrim(g[n[j]].base,2), form='(i7,a5,2x,f9.3,f10.1,2x,a)'
	 for j=0,countnuv-1 do printf,cl,'cp '+strtrim(g[n[j]].info_str,2)+ $
		 '/'+strtrim(g[n[j]].base,2)+'-nd-{int,cnt,rrhr}.fit* .'
	 printf,cl,'cd ..'
 endif
endfor 
free_lun,ol
print,' '
if keyword_set(extra) then free_lun,cl
end
