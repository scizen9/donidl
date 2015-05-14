pro galdb_updt_glga,silent=silent,verbose=verbose
;+
;	update the GLGA apphot mags
;-
; common variable for galdat
COMMON galdb_info, galdat, gphsrc
COMMON glgadb_info, glgadat
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; new record
A = {galdb_data}
A = struct_init(A)
added = 0L
updated = 0L
;
; read in prieto08 data
prdat = read_prieto08(silent=silent,verbose=verbose)
;
; read in nyu data
sdss = read_nyu(silent=silent,verbose=verbose)
;
; loop over glgadat struct
ngl = n_elements(glgadat)
if not keyword_set(silent) or keyword_set(verbose) then $
	print,'Checking this many GLGA galaxies: ',ngl
for i=0L,ngl-1L do begin
        ;
        ; print status
        if keyword_set(verbose) then $
                print,string(13B),i+1,'/',ngl,glgadat[i].id, $
                        format='($,a1,i7,a,i7,2x,a-32)'
	rec_updated = (1 eq 0)
	;
	; are we in galdat?
	p=gfind(glgadat[i].id,count=nmhf,ra=glgadat[i].ra,dec=glgadat[i].dec, $
		srad=10.,stat=stat,/silent)
	p = p[0]
	;
	; if not add a record
	if nmhf le 0 then begin
		galdat = [galdat,A]
		p = n_elements(galdat)-1L
		galdat[p].id		= get_hl_name(glgadat[i].id)
		galdat[p].ra		= glgadat[i].ra
		galdat[p].dec		= glgadat[i].dec
		galdat[p].majax		= glgadat[i].majax
		galdat[p].minax		= glgadat[i].minax
		galdat[p].pa		= glgadat[i].pa
		galdat[p].coo_src	= 'G'
		glactc,galdat[p].ra,galdat[p].dec,2000.,gall,galb,1,/degree
		galdat[p].mwebmv	= dust_getval(gall,galb)
		galdat[p].sample	= $
			gadd_sample(galdat[p].sample,'glga')
		galdat[p].mod_time	= systime(1)
		;
		; update published data
		galdb_updt_pubdat,p,silent=silent,verbose=verbose
		;
		; update NYU value-added catalog DR6
		galdb_updt_nyu,p,silent=silent,verbose=verbose,sdss=sdss
		;
		; update Prieto08 abundance data
		galdb_updt_prieto08,p,silent=silent,verbose=verbose,prdat=prdat
		;
		if keyword_set(verbose) then $
			print,'New record for: ',glgadat[i].id,' ',galdat[p].id
		added = added + 1L
	endif
	;
	; check for non-id match and add id to altids list
	if stat ge 1 then begin
		galdat[p].altids = gadd_name(galdat[p].altids,glgadat[i].id)
		rec_updated = (1 eq 1)
	endif
	;
	; check coords and shape params
	if galdat[p].ra lt 0. then begin
		galdat[p].ra		= glgadat[i].ra
		galdat[p].dec		= glgadat[i].dec
		galdat[p].majax		= glgadat[i].majax
		galdat[p].minax		= glgadat[i].minax
		galdat[p].pa		= glgadat[i].pa
		galdat[p].coo_src	= 'G'
		if strlen(galdat[p].type) eq 0 then $
			galdat[p].type	= glgadat[i].type
		glactc,galdat[p].ra,galdat[p].dec,2000.,gall,galb,1,/degree
		galdat[p].mwebmv	= dust_getval(gall,galb)
		galdat[p].mod_time	= systime(1)
		;
		; update published data
		galdb_updt_pubdat,p,silent=silent,verbose=verbose
		;
		; update NYU value-added catalog DR6
		galdb_updt_nyu,p,silent=silent,verbose=verbose,sdss=sdss
		;
		; update Prieto08 abundance data
		galdb_updt_prieto08,p,silent=silent,verbose=verbose,prdat=prdat
		;
		rec_updated = (1 eq 1)
	endif
	;
	; did we update this record?
	if rec_updated then begin
		updated = updated + 1L
		if keyword_set(verbose) then $
			print,'Updated record for: ',glgadat[i].id,' ',galdat[p].id
	endif
endfor
;
print,' '
print,'Added this many galdat records  : ',added
print,'Updated this many galdat records: ',updated
;
return
end
