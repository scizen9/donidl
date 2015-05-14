pro sndb_create,silent=silent,verbose=verbose
;+
;	gather all the sn data into a master structure
;-
; common variables for sndat
COMMON sndb_info, sndat
;
; read asiago catalog
read_asiago_sn,id,host,hra,hdec,ra,dec,htype,htyn,hinc,hpa,vz,hmag,hlgd25,$
		off_ew,off_ns,filt,mag,mag_type,type,date,disc,silent=silent
read_sai_sn,dsn,dhost,dhra,dhdec,sna,snd,/silent
read_snadcomp,csn,choi,/silent
;
; define sn structure: see sndb_data__define for definition
A = {sndb_data}
A = struct_init(A)
;
; sndat
nsne=n_elements(id)
sndat=replicate(A,nsne)
;
; calculate cz
cz=vz
t=where(vz gt 0. and vz lt 2.)
cz[t] = vz[t] * !phys_c
;
; populate asiago data
for i=0L,nsne-1L do begin
;
; print status
	if keyword_set(verbose) then $
	    print,string(13B),i+1,'/',nsne,id[i],format='($,a1,i5,a,i5,2x,a-8)'
;
	sndat[i].id		= string(id[i],format='(a-8)')
	sndat[i].iau_id		= string(id[i],format='(a-8)')
	sndat[i].srv_id		= string(id[i],format='(a-8)')
	sndat[i].srv_name	= 'IAU'
	sndat[i].type		= string(type[i],format='(a-8)')
	sndat[i].tyn		= get_sntyn(type[i])
;
; Asiago coords are default
	sndat[i].ra		= ra[i]
	sndat[i].dec		= dec[i]
	sndat[i].coo_src	= 'A'
;
; use SAI coords if no Asiago coords available
	if ra[i] lt 0. or dec[i] lt -90. then begin
		d=where(strpos(dsn,id[i]) ge 0, nd)
		if nd gt 0 then begin
			d=d[0]
			sndat[i].ra	= sna(d)
			sndat[i].dec	= snd(d)
			sndat[i].coo_src= 'S'
		endif
	endif else begin
;
; check comparison between Asiago and SAI if both available
		w=where(strpos(csn,id[i]) ge 0, nw)
		if nw eq 1 then begin
			w=w[0]
			;
			; use SAI
			if choi[w] eq 'S' then begin
				d=where(strpos(dsn,id[i]) ge 0, nd)
				if nd gt 0 then begin
					d=d[0]
					sndat[i].ra	= sna(d)
					sndat[i].dec	= snd(d)
					sndat[i].coo_src= 'S'
				endif
			;
			; use host offset
			endif else if choi[w] eq 'O' then begin
				adoffset,hra[i],hdec[i],off_ew[i],off_ns[i],0, $
					ra1,dec1
				sndat[i].ra	= ra1
				sndat[i].dec	= dec1
				sndat[i].coo_src= 'O'
			endif
		endif
	endelse
;
; host info
	sndat[i].host		= get_hl_name(host[i])
	sndat[i].hra		= hra[i]
	sndat[i].hdec		= hdec[i]
	sndat[i].hcoo_src	= 'A'
	sndat[i].hpa		= hpa[i]
	sndat[i].hinc		= hinc[i]
	sndat[i].hmajax		= 10.^hlgd25[i]*6.
	sndat[i].hminax		= sndat[i].hmajax * $
		sqrt(0.04 + 0.96*cos((hinc[i]-3.)/!radeg)^2)
;
; SN basic data
	sndat[i].cz		= cz[i]
	sndat[i].off_ew		= off_ew[i]
	sndat[i].off_ns		= off_ns[i]
	sndat[i].filt		= string(filt[i],format='(a-3)')
	sndat[i].mag		= mag[i]
	sndat[i].mag_type	= string(filt[i],format='(a1)')
	sndat[i].date		= string(date[i],format='(a-12)')
	sndat[i].discoverer	= string(disc[i],format='(a-36)')
	glactc,sndat[i].ra,sndat[i].dec,2000.,gall,galb,1,/degree
	sndat[i].mwebmv		= dust_getval(gall,galb)
;
; get isophotal and equivalent radii
	if sndat[i].off_ew gt -9000. and sndat[i].off_ns gt -9000. then begin
		if hinc[i] gt 0. and hpa[i] gt 0. then begin
			riso = isorad(hpa[i],sndat[i].off_ew, $
				      sndat[i].off_ns,inc=hinc[i], $
				      b=b,/silent)
			sndat[i].r_iso	= riso
			sndat[i].r_equiv= sqrt(riso*b)	; equivalent radius
		endif else begin
			sndat[i].r_iso	= sqrt(sndat[i].off_ew^2 + $
					       sndat[i].off_ns^2)
			sndat[i].r_equiv= sndat[i].r_iso
		endelse
	endif
	;
	; time stamp
	sndat[i].mod_time	= systime(1)
;
; end populate asiago data.
endfor
print,' '
;
; add supernovae from other surveys (non-IAU)
sndb_updt_snsurvey,nsne,verbose=verbose,silent=silent
;
; fix any data (we use /nosave in case there is nothing to fix)
sndb_catfix,/nosave
;
; update SN site data
sndb_updt_site,verbose=verbose,silent=silent
;
; update profile analysis data
sndb_updt_prof,verbose=verbose,silent=silent
;
; update light curve data
sndb_updt_lcfit,verbose=verbose,silent=silent
;
; save file
savfile=!SNE_DATA+'/sndb_info.sav'
;
; mv old save file
filestamp,savfile,/verbose
;
; create save file
print,'Saving SNe info to: ',savfile
save,sndat,filename=savfile,/verbose
;
return
end
