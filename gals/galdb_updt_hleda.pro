pro galdb_updt_hleda,silent=silent,verbose=verbose
;+
;	update galdat struct from Hyper LEDA data
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
;
; Hyper LEDA
restore,!CAT_DATA+'hl_master_structs.sav'
;
; galdat
ngal=n_elements(galdat)
;
; error log
efile='galdb_updt_hleda.log'
filestamp,efile,/arch
openw,el,efile,/get_lun
printf,el,'# GALDB_UPDT_HLEDA - '+systime(0)
;
; loop over database
for i=0,ngal-1 do begin
	;
	; flags
	hlnb = 0	; neighbor
	hlpd = 0	; position offset
	m = -1L
	;
	; get host name matches
	host=strtrim(galdat[i].id,2)
	hlhost=get_hl_name(host)
	h=where(strcmp(hldata.objname,hlhost) eq 1, nh)
	;
	; get position matches
	gcirc,2,galdat[i].ra,galdat[i].dec,hldata.al2000*15.d0,hldata.de2000,r
	rlim = (galdat[i].majax/2.0)>2.0
	w=where(r le rlim, nw)
	;
	; unambiguous host name match
	if nh eq 1 then begin
		m = h[0]
		;
		; no coord match
		if nw eq 0 then $
			hlpd = 1	; pos err
		;
		; multiple coord matches
		if nw gt 1 then $
			hlnb = 1	; neighbor
	;
	; ambiguous or no host name match
	endif else if nh gt 1 or nh le 0 then begin
		;
		; unambiguous coord match
		if nw eq 1 then begin
			m = w[0]
		;
		; more than one match
		endif else if nw gt 1 then begin
			;
			; get closest in position
			r=r[w]
			s=sort(r)
			w=w[s]
			m = w[0]
			hlnb = 1
		endif
	;
	; no host name match
	endif
	if hlnb eq 1 then $
		printf,el,'Host w/neighbr: ',host,' ',hlhost
	if hlpd eq 1 then $
		printf,el,'Host coord err: ',host,' ',hlhost,$
				galdat[i].ra,galdat[i].dec, $
				format='(a,a,a,a,2f13.8)'
	;
	; get hldata
	galdat[i].hlind = m
	if m ge 0 then begin
		galdat[i].hlname = hldata[m].objname
		galdat[i].pgc = hldata[m].pgc
		galdat[i].hltype = hldata[m].type
		galdat[i].hltyn = hldata[m].t
		galdat[i].hlneighbor = hlnb
		galdat[i].hlposdiff = hlpd
		alts = hldata[m].altnames
		galdat[i].hlalt1 = gettok(alts,',')
		galdat[i].hlalt2 = gettok(alts,',')
	;
	; given becomes cononical
		galdat[i].ra = hldata[m].al2000*15.d0
		galdat[i].dec= hldata[m].de2000
		galdat[i].majax = 10.^(hldata[m].logd25) * 6.	; arcsec
		galdat[i].minax = galdat[i].majax/10.^(hldata[m].logr25)
		if finite(hldata[m].pa) eq 0 then $
			galdat[i].pa = -100. $
		else	galdat[i].pa = hldata[m].pa
	;
	; given params
		galdat[i].g_ra = hldata[m].al2000*15.d0
		galdat[i].g_dec= hldata[m].de2000
		galdat[i].g_majax = 10.^(hldata[m].logd25) * 6.	; arcsec
		galdat[i].g_minax = galdat[i].g_majax/10.^(hldata[m].logr25)
		if finite(hldata[m].pa) eq 0 then $
			galdat[i].g_pa = -100. $
		else	galdat[i].g_pa = hldata[m].pa
		galdat[i].sample = gadd_sample(galdat[i].sample,'hleda')
		galdat[i].mod_time = systime(1)
	endif else $
		printf,el,'Host not found: ',host,' ',hlhost
	print,string(13B),i+1,'/',ngal,host,format='($,a1,i5,a,i5,2x,a-32)'
endfor
free_lun,el
print,' '
;
return
end
