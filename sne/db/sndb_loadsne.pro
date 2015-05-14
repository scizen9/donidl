pro sndb_loadsne,silent=silent,verbose=verbose
;
;	gather all the sn data into a master structure
;
; useful constants
om=0.27		; Omega matter
ol=0.73		; Omega Lambda
h0=73.0		; Hubble constant
;
; db connections
dbstr="dbname='asncat'"
acon=obj_new("pgSqlCon",dbstr)
dbstr="dbname='saisncat'"
scon=obj_new("pgSqlCon",dbstr)
dbstr="dbname='sndb'"
ncon=obj_new("pgSqlCon",dbstr)
;
; get coordinate choices
read_snadcomp,csn,choi,/silent
;
; get list of SNe
if sndb_get_column('sn.sn','id',id,dbconn=scon,verbose=verbose) $
    then begin
	print,'SNDB_LOADSNE: Error - cannot extract SN list from saisncat db'
	return
endif
nsne=n_elements(id)
;
; get sn catalog indices
res = sndb_get_column('sn.sncats','id',sncid)
res = sndb_get_column('sn.sncats','name',sncats)
asicat = where(strpos(sncats,'ASIAGO') ge 0)+1
saicat = where(strpos(sncats,'SAI') ge 0)+1
offcat = where(strpos(sncats,'OFFSET') ge 0)+1
;
; get galaxy catalog indices (only NED for this pass)
res = sndb_get_column('sn.gcats','id',gid)
res = sndb_get_column('sn.gcats','name',gcats)
nedcat = where(strpos(gcats,'NED') ge 0)+1
;
; populate sndb
for i=0,nsne-1 do begin
;
; is this a new SN?
	if not snid_exists(id(i),/silent) then begin
;
; print status
		if keyword_set(verbose) then $
		    print,string(13B),i+1,'/',nsne,id(i), $
		    	format='($,a1,i5,a,i5,2x,a-8)'
;
; get sai catalog data
		sairec	= sndb_get_sairec(id(i))
;
; get asiago catalog data
		asirec	= sndb_get_asirec(id(i))
;
; fill easy values
		sr	= {snrec_load}
		sr.id	= id(i)
		sr.name	= sairec.name
		sr.tyn	= sairec.type
;
; get type string from Asiago
		sr.type = asirec.type
;
; get cz of SN from Asiago db
		sr.cz	= asirec.cz
;
; Asiago coords are default
		sr.ra	= asirec.ra
		sr.dec	= asirec.dec
		sr.scid	= asicat
;
; use SAI coords if no Asiago coords available
		if sr.ra lt 0. or sr.dec lt -90. then begin
			sr.ra	= sairec.ra
			sr.dec	= sairec.dec
			sr.scid	= saicat
		endif else begin
;
; check comparison between Asiago and SAI if both available
			w=where(strpos(csn,name(i)) ge 0, nw)
			if nw eq 1 then begin
				w=w(0)
			;
			; use SAI
				if choi(w) eq 'S' then begin
					sr.ra	= sairec.ra
					sr.dec	= sairec.dec
					sr.scid	= saicat
			;
			; use host offset
				endif else if choi(w) eq 'O' then begin
					hra = asirec.hra
					hdec= asirec.hdec
					oew = asirec.off_ew
					ons = asirec.off_ns
					adoffset,hra,hdec,oew,ons,0,ra1,dec1
					sr.ra	= ra1
					sr.dec	= dec1
					sr.scid	= offcat
				endif
			endif
		endelse
;
; offsets
		sr.off_ew	= asirec.off_ew
		sr.off_ns	= asirec.off_ns
;
; end if sn id exists
	endif
;
; add new record
	if sndb_insert_rec('sn.sn',sr,dbconn=ncon) then $
	    print,'SNDB_LOADSNE: Error - unable to insert record for: ',id(i)
;
; end populate asiago data.
endfor
;
; destroy objects
obj_destroy, [acon,scon,ncon]
;
return
end
