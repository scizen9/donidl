pro sndb_addsne,silent=silent,verbose=verbose
;+
; sndb_addsne - add new sne to dbase
;-
; common variable for sndat
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
nnsne=n_elements(id)
nosne=n_elements(sndat)
;
; calculate cz
cz=vz
t=where(vz gt 0. and vz lt 2.)
cz[t] = vz[t] * !phys_c
;
; are there new ones?
newsn = nnsne - nosne
if newsn gt 0 then begin
;
; how many new ones?
    added = 0L	; SNe
;
; loop over asiago data
    for i=0L,nnsne-1L do begin
;
; are we new?
	p = snfind(id[i],/silent)
	if p lt 0 then begin
		p = nosne
		nosne = nosne + 1L
		added = added + 1L
		sndat = [sndat,A]	; add new record
;
; print status
		if keyword_set(verbose) then $
			print,string(13B),added,'/',newsn,id[i], $
				format='($,a1,i5,a,i5,2x,a-8)'
;
		sndat[p].id		= string(id[i], format='(a-8)')
		sndat[p].iau_id		= string(id[i], format='(a-8)')
		sndat[p].srv_id		= string(id[i], format='(a-8)')
		sndat[p].srv_name	= 'IAU'
		sndat[p].type		= string(type[i], format='(a-8)')
		sndat[p].tyn		= get_sntyn(type[i])
;
; Asiago coords are default
		sndat[p].ra		= ra[i]
		sndat[p].dec		= dec[i]
		sndat[p].coo_src	= 'A'
;
; use SAI coords if no Asiago coords available
		if ra[i] lt 0. or dec[i] lt -90. then begin
			d=where(strpos(dsn,id[i]) ge 0, nd)
			if nd gt 0 then begin
				d=d[0]
				sndat[p].ra	= sna[d]
				sndat[p].dec	= snd[d]
				sndat[p].coo_src= 'S'
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
						sndat[p].ra	= sna[d]
						sndat[p].dec	= snd[d]
						sndat[p].coo_src= 'S'
					endif
				;
				; use host offset
				endif else if choi[w] eq 'O' then begin
					adoffset,hra[i],hdec[i], $
						off_ew[i],off_ns[i],0,ra1,dec1
					sndat[p].ra	= ra1
					sndat[p].dec	= dec1
					sndat[p].coo_src= 'O'
				endif
			endif
		endelse
;
; host basic data
		sndat[p].host		= get_hl_name(host[i])
		sndat[p].hra		= hra[i]
		sndat[p].hdec		= hdec[i]
		sndat[p].hcoo_src	= 'A'
		sndat[p].hpa		= hpa[i]
		sndat[p].hinc		= hinc[i]
		sndat[p].hmajax		= 10.^hlgd25[i]*6.
		sndat[p].hminax		= sndat[p].hmajax * $
			sqrt(0.04 + 0.96*cos((hinc[i]-3.)/!radeg)^2)
;
; SN basic data
		sndat[p].cz		= cz[i]
		sndat[p].off_ew		= off_ew[i]
		sndat[p].off_ns		= off_ns[i]
		sndat[p].filt		= string(filt[i],format='(a-3)')
		sndat[p].mag		= mag[i]
		sndat[p].mag_type	= string(filt[i],format='(a1)')
		sndat[p].date		= string(date[i],format='(a-12)')
		sndat[p].discoverer	= string(disc[i],format='(a-36)')
		glactc,sndat[p].ra,sndat[p].dec,2000.,gall,galb,1,/degree
		sndat[p].mwebmv		= dust_getval(gall,galb)
;
; get isophotal and equivalent radii
		if sndat[p].off_ew gt -9000. and sndat[p].off_ns gt -9000. $
		    then begin
		    if hinc[i] gt 0. and hpa[i] gt 0. then begin
			riso = isorad(hpa[i],sndat[p].off_ew,sndat[p].off_ns, $
					inc=hinc[i],b=b,/silent)
			sndat[p].r_iso	= riso
			sndat[p].r_equiv= sqrt(riso*b)	; equiv. radius
		    endif else begin
			sndat[p].r_iso	= sqrt(sndat[p].off_ew^2 + $
					       sndat[p].off_ns^2)
			sndat[p].r_equiv= sndat[p].r_iso
		    endelse
		endif
;
; end if we are new
	endif
;
; end loop over asiago data.
    endfor
    print,' '
endif	; are there new ones?
print,' '
;
return
end
