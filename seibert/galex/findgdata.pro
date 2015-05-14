pro findgdata, ra, dec, radius,  $
               id = id, outfile = outfile,  pathfile = pathfile, $
               qagrade = qagrade,  notqagrade = notqagrade, $
               reportnodata = reportnodata, simpleheader = simpleheader

;; Find galex data using the mission status file

if not keyword_set(radius) then radius = fltarr(n_elements(ra))+36 ;arcminutes

if n_elements(ra) gt 1 and n_elements(radius) eq 1 then $
  radius = fltarr(n_elements(ra))+radius

if not keyword_set(id) then id = strarr(n_elements(ra))


if n_elements(ra) ne n_elements(dec) or $
 n_elements(ra) ne n_elements(id) then begin
 print, 'Supplied arrays must be of same size'
 return
endif

if not keyword_set(outfile) then outfile = '/dev/tty'
openw,lun,outfile,/get_lun, /more

if keyword_set(pathfile) then begin
 openw,lun2,pathfile,/get_lun
 !textunit=lun2
endif

hdr=headfits('~/mission-status/mission_status.fits',ext = 0)
msdate = sxpar(hdr, 'RUNDAT')
gall=mrdfits('~/mission-status/mission_status.fits',1)
gall.nuv_exptime = gall.nuv_exptime > 0
gall.fuv_exptime = gall.fuv_exptime > 0
gall.surv_type = strcompress(gall.surv_type,/rem)

openw,lun,outfile,/get_lun, /more

printf,lun,'# FINDGDATA: '+systime()
printf,lun,'# Mission-Status File Date: '+msdate

if keyword_set(simpleheader) then begin
printf,lun,"#================================================================================================================="
printf,lun,"# SEARCH radius(')="+string(radius[0], format = '(f7.2)') 
if keyword_set(qagrade) then printf,lun,"# QAGRADE=", qagrade
if keyword_set(notqagrade) then printf,lun,"# NOTQAGRADE=", notqagrade
if not keyword_set(notqagrade) and not keyword_set(qagrade) then printf,lun,"#"
printf,lun,"#                                              FUVtime    NUVtime   Rdist          RA        DEC"
printf,lun,"#                    Tile  Leg Nvis  Srv OW    tot (s)    tot (s)     (')     (J2000)     (J2000)               ID"
printf,lun,"#-----------------------------------------------------------------------------------------------------------------"
endif



for r = 0, n_elements(ra)-1 do begin

gcirc, 1, ra[r]/15., dec[r], gall.ra0/15., gall.dec0,  dist

if not keyword_set(qagrade) then $
a = where(dist/60. le radius[r] and $
        (gall.nuv_exptime gt 0 or gall.fuv_exptime gt 0)and $
        gall.surv_type ne 'SIO' and gall.surv_type ne 'SSO' and $
        gall.surv_type ne 'SOO',  counta)

if keyword_set(qagrade) then $
a = where(dist/60. le radius[r] and $
        (gall.nuv_exptime gt 0 or gall.fuv_exptime gt 0)and $
        gall.surv_type ne 'SIO' and gall.surv_type ne 'SSO' and $
        gall.surv_type ne 'SOO' and $
        strcompress(gall.qa_grade, /rem) eq qagrade, counta)
 
if keyword_set(notqagrade) then $
a = where(dist/60. le radius[r] and $
        (gall.nuv_exptime gt 0 or gall.fuv_exptime gt 0)and $
        gall.surv_type ne 'SIO' and gall.surv_type ne 'SSO' and $
        gall.surv_type ne 'SOO' and $
        strcompress(gall.qa_grade, /rem) ne notqagrade, counta)


if (counta eq 0  and keyword_set(reportnodata) and not keyword_set(simpleheader)) or (counta gt 0 and not keyword_set(simpleheader)) then begin
printf,lun,"#================================================================================================================="
printf,lun,"# SEARCH "+strcompress(string(r+1), /rem)+" of "+strcompress(string(n_elements(ra)), /rem)+": radius(')="+string(radius[r], format = '(f7.2)')
if keyword_set(qagrade) then printf,lun,"# QAGRADE=", qagrade
if keyword_set(notqagrade) then printf,lun,"# NOTQAGRADE=", notqagrade
if not keyword_set(notqagrade) and not keyword_set(qagrade) then printf,lun,"#"
printf,lun,"#                                              FUVtime    NUVtime   Rdist         RA          Dec"
printf,lun,"#                    Tile  Leg Nvis  Srv OW    tot (s)    tot (s)     (')     (J2000)     (J2000)               ID"
printf,lun,"#-----------------------------------------------------------------------------------------------------------------"
endif

if counta eq 0 then begin
 if keyword_set(reportnodata) then begin
   printf,lun, "### "+id[r]+": no data found ###"
   printf,lun, ""
 endif
 goto, skipposition
endif

g = gall[a]
dist = dist[a]/60 ; distance in arcmins from FOV center

fld=strarr(n_elements(g))
for ss=0l,n_elements(fld)-1 do $
  fld[ss]=strcompress(g[ss].tile,/rem)+$
  '_'+strn(g[ss].visit,padchar='0',len=4,padtype=1)

s=where(g.leg ne 0, count)
if count gt 0 then begin
  for ss=0l,count-1 do $
   fld[s[ss]]=fld[s[ss]]+'_sv'+$
   strn(g[s[ss]].sub_visit,padchar='0',len=2,padtype=1)
endif


us=uniq(g.surv_type,sort(g.surv_type))
surv=strcompress(g[us].surv_type,/rem)

for i=0,n_elements(surv)-1 do begin

 a=where(strpos(g.surv_type,surv[i]) ge 0  and $
         (g.nuv_exptime gt 0 or g.fuv_exptime gt 0), counta)

 if counta eq 0 then goto, skipout

 tl=strcompress(g[a].tile,/rem)+'-L'+strcompress(g[a].leg,/rem)

 u=uniq(tl,sort(tl))

 for j=0,n_elements(u)-1 do begin

  t=where(tl eq tl[u[j]])
  printf,lun, strcompress(g[a[t[0]]].tile,/rem),g[a[t[0]]].leg,n_elements(t),$
          strcompress(g[a[t[0]]].surv_type,/rem),g[a[t[0]]].ow, $ 
          total(g[a[t]].fuv_exptime), total(g[a[t]].nuv_exptime),$
          dist[a[t[0]]], $
          ;g[a[t[0]]].ra0, g[a[t[0]]].dec0, $
          ra[r], dec[r],id[r], $
    format='(a25,2x,i3,2x,i3,2x,a3,2x,a1,2x,f9.2,2x,f9.2,2x,f6.2,2x,f10.6,2x,f10.6,2x,a15)'


 endfor

 skipout:

;free_lun,lun

endfor 

if keyword_set(simpleheader) then printf,lun,""

skipposition:

endfor 

free_lun,lun

end
