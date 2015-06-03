pro get_ned,obj,radeg=radeg,decdeg=decdeg,rasex=rasex,decsex=decsex,pgc=pgc, $
	galldeg=galldeg,galbdeg=galbdeg,rvhel=rvhel,rverr=rverr,z=z,errz=errz,$
	majdim=majdim,mindim=mindim,mag=mag,filt=filt,class=class,$
	vgal=vgal,vlg=vlg,v3k=v3k,dgal=dgal,dlg=dlg,d3k=d3k,$
	mugal=mugal,mulg=mulg,mu3k=mu3k,sclgal=sclgal,scllg=scllg,scl3k=scl3k,$
	coslumd=coslumd,coslummu=coslummu,cosangd=cosangd,cosangmu=cosangmu,$
	coradd=coradd,cosradmu=cosradmu,cotand=cotand,costanmu=costanmu,$
	cosscl=cosscl,sbdimflx=sbdimflx,sbdimmag=sbdimmag,$
	mwextbh=mwextbh,mwextsch=mwextsch,mwebmv=mwebmv,$
	mwextsfb=mwextsfb,mwextsfi=mwextsfi,mwextsfl=mwextsfl, $
	mwband=mwband,mwalam=mwalam,$
	reread=reread,silent=silent
;
; see if it's in cache
nedir=!NED_CACHE
flist=file_search(nedir+'/'+obj+'.html',count=nf)
;
; if not, use wget to retrieve NED data
if nf lt 1 or keyword_set(reread) then begin
    cmd='wget "http://ned.ipac.caltech.edu/cgi-bin/nph-objsearch?objname='+$
	    obj+' & extend=no & out_csys=Equatorial & out_equinox=J2000.0 & obj_sort=RA+or+Longitude & of=pre_text & zv_breaker=30000.0 & list_limit=5 & img_stamp=NO" -O '+nedir+'/'+obj+'.html'
    if not keyword_set(silent) then begin
	    print,'wgetting from NED...'
	    print,cmd
    endif
    spawn,cmd,res,eres
endif
;
; get file
flist=file_search(nedir+'/'+obj+'.html',count=nf)
if nf eq 1 then begin
;
; read in file
	openr,il,flist[0],/get_lun
	rec=''
	page=''
	while not eof(il) do begin
		readf,il,rec
		page=[page,rec]
	endwhile
	page=page[1:*]
	free_lun,il
;
; check output
	t=where(strpos(page,'not currently recognized') ge 0, n)
	if n gt 0 then begin
		if not keyword_set(silent) then $
			print,obj+' not found in NED'
		prt = (1 eq 0)
	endif else prt = (1 eq 1)
	if keyword_set(silent) then prt = (1 eq 0)
;
; get data
;
; RA,DEC
	t=where(strpos(page,'Equatorial (J2000.0)') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,2 do val=gettok(rec,' ')
		radeg = double(val)
		val=gettok(rec,' ')
		decdeg = double(val)
		rasex = gettok(rec,' ')
		decsex = gettok(rec,' ')
	endif else begin
		if prt then print,'Equatorial (J2000.0) coords not found'
		radeg=-9.
		decdeg=-99.
	endelse
;
; Galactic l,b
	t=where(strpos(page,'Galactic     ') ge 0, n)
	if n gt 0 then begin
		rec=page[t[0]]
		for i=0,1 do val=gettok(rec,' ')
		galldeg = double(val)
		galbdeg = double(gettok(rec,' '))
	endif else begin
		if prt then print,'Galactic coords not found'
		galldeg = -9.
		galbdeg = -99.
	endelse
;
; PGC number
	t=where(strpos(page,'>PGC<') ge 0, n)
	if n gt 0 then begin
		rec=page[t[0]]
		pstr=strmid(rec,strpos(rec,'>PGC<')+8)
		val=gettok(pstr,' ')
		pgc = long(val)
	endif else begin
		;
		; check LEDA number
		t=where(strpos(page,'>LEDA<') ge 0, n)
		if n gt 0 then begin
			rec=page[t[0]]
			pstr=strmid(rec,strpos(rec,'>LEDA<')+9)
			val=gettok(pstr,' ')
			pgc = long(val)
		endif else begin
			;
			; check pgc in filename
			if strpos(obj,'PGC') ge 0 or $
			   strpos(obj,'LEDA') ge 0 then begin
				if strpos(obj,'PGC') ge 0 then $
					pgc = long(strmid(obj,3)) $
				else	pgc = long(strmid(obj,4))
			endif else begin
				if prt then print,'PGC number not found'
				pgc = -9ll
			endelse
		endelse
	endelse
;
; Helio. Radial Velocity
	t=where(strpos(page,'Helio. Radial Velocity') ge 0, n)
	if n gt 0 then begin
		rec=page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,rvhel) then $
			rvhel = -9999.9
		for i=0,1 do val=gettok(rec,' ')
		if not valid_num(val,rverr) then $
			rverr = -9.
	endif else begin
		if prt then print,'Helio. radial velocity not found'
		rvhel=-9999.9
		rverr=-9.
	endelse
;
; Redshift
	t=where(strpos(page,'Redshift       ') ge 0, n)
	if n gt 0 then begin
		rec=page[t[0]]
		for i=0,2 do val=gettok(rec,' ')
		if not valid_num(val,z) then $
			z=-99.
		for i=0,1 do val=gettok(rec,' ')
		if not valid_num(val,errz) then $
			errz=-9.
	endif else begin
		if prt then print,'Redshift not found'
		z=-99.
		errz=-9.
	endelse
;
; Major Diameter
	t=where(strpos(page,'Major Diameter') ge 0, n)
	if n gt 0 then begin
		rec=page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,majdim) then $
			majdim=-9.
	endif else begin
		if prt then print,'Major Diameter not found'
		majdim=-9.
	endelse
;
; Minor Diameter
	t=where(strpos(page,'Minor Diameter') ge 0, n)
	if n gt 0 then begin
		rec=page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,mindim) then $
			mindim=-9.
	endif else begin
		if prt then print,'Minor Diameter not found'
		mindim=-9.
	endelse
;
; Magnitude and Filter
	t=where(strpos(page,'Magnitude and Filter') ge 0, n)
	if n gt 0 then begin
		rec=page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,mag) then $
			mag=-99.
		filt=gettok(rec,' ')
	endif else begin
		if prt then print,'Magnitude and Filter not found'
		mag=-99.
		filt=''
	endelse
;
; Classifications
	t=where(strpos(page,'Classifications  ') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,1 do val=gettok(rec,' ')
		rec=strtrim(strcompress(rec),2)
		while strpos(rec,' ') ge 0 do strput,rec,';',strpos(rec,' ')
		class=rec
	endif else begin
		if prt then print,'Classifications not found'
		class=''
	endelse
;
; DERIVED VALUES

; V (Galactocentric GSR)
	t=where(strpos(page,'V (Galactocentric GSR)') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,vgal) then $
			vgal=-9.
	endif else begin
		if prt then print,'V (Galactocentric GSR) not found'
		vgal=-9.
	endelse
;
; V (Local Group)
	t=where(strpos(page,'V (Local Group)') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,vlg) then $
			vlg=-9.
	endif else begin
		if prt then print,'V (Local Group) not found'
		vlg=-9.
	endelse
;
; V (3K CMB)
	t=where(strpos(page,'V (3K CMB)') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,v3k) then $
			v3k=-9.
	endif else begin
		if prt then print,'V (3K CMB) not found'
		v3k=-9.
	endelse
;
; D (Galactocentric GSR)
	t=where(strpos(page,'D (Galactocentric GSR)') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,dgal) then $
			dgal=-9.
	endif else begin
		if prt then print,'D (Galactocentric GSR) not found'
		dgal=-9.
	endelse
;
; D (Local Group)
	t=where(strpos(page,'D (Local Group)') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,dlg) then $
			dlg=-9.
	endif else begin
		if prt then print,'D (Local Group) not found'
		dlg=-9.
	endelse
;
; D (3K CMB)
	t=where(strpos(page,'D (3K CMB)') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,4 do val=gettok(rec,' ')
		if not valid_num(val,d3k) then $
			d3k=-9.
	endif else begin
		if prt then print,'D (3K CMB) not found'
		d3k=-9.
	endelse
;
; Angular-Size Distance
	t=where(strpos(page,'Angular-Size Distance') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,3 do val=gettok(rec,' ')
		if not valid_num(val,cosangd) then $
			cosangd=-1.
		for i=0,3 do val=gettok(rec,' ')
		if not valid_num(val,cosangmu) then $
			cosangmu=-9.
	endif else begin
		if prt then print,'Angular-Size Distance not found'
		cosangd=-1.
		cosangmu=-9.
	endelse
;
; Luminosity Distance
	t=where(strpos(page,'Luminosity Distance') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,3 do val=gettok(rec,' ')
		if not valid_num(val,coslumd) then $
			coslumd=-1.
		for i=0,3 do val=gettok(rec,' ')
		if not valid_num(val,coslummu) then $
			coslummu=-9.
	endif else begin
		if prt then print,'Luminosity Distance not found'
		coslumd=-1.
		coslummu=-9.
	endelse
;
; Scale
	t=where(strpos(page,'Scale (Cosmology Corrected)') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,3 do val=gettok(rec,' ')
		if not valid_num(val,cosscl) then $
			cosscl=-1.
	endif else begin
		if prt then print,'Scale (Cosmology Corrected) not found'
		cosscl=-1.
	endelse
;
; Galactic Extinction (Schlafly & Finkbeiner 2011)
	t=where(strpos(page,'Schlafly & Finkbeiner') ge 0, n)
	if n gt 0 then begin
		rec = page(t(0)+4)
		rec = strmid(rec,strpos(rec,']')+1)
		sta = strsplit(rec,'</?td>',/extract,/regex)
		; B
		val = sta[1]
		if not valid_num(val,mwextsfb) then $
			mwextsfb=-9.
		; I
		val = sta[4]
		if not valid_num(val,mwextsfi) then $
			mwextsfi=-9.
		; L
		val = sta[13]
		if not valid_num(val,mwextsfl) then $
			mwextsfl=-9.
	endif else begin
		if prt then print,'Galactic Extinction (Schlafly & Finkbeiner) not found'
		mwextsfb=-9.
		mwextsfi=-9.
		mwextsfl=-9.
	endelse
;
; Galactic Extinction (Burstein & Heiles)
	t=where(strpos(page,'Burstein & Heiles') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		rec = strmid(rec,strpos(rec,'A_B'))
		for i=0,2 do val=gettok(rec,' ')
		if not valid_num(val,mwextbh) then $
			mwextbh=-9.
	endif else begin
		if prt then print,'Galactic Extinction (Burstein & Heiles) not found'
		mwextbh=-9.
	endelse
;
; Galactic Extinction (Schlegel et al.)
	t=where(strpos(page,'(Schlegel et al.)') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		rec = strmid(rec,strpos(rec,'A_B'))
		for i=0,2 do val=gettok(rec,' ')
		if not valid_num(val,mwextsch) then $
			mwextsch=-9.
	endif else begin
		if prt then print,'Galactic Extinction (Schlegel et al.) not found'
		mwextsch=-9.
	endelse
;
; Color excess
	t=where(strpos(page,' E(B-V) ') ge 0, n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,2 do val=gettok(rec,' ')
		if not valid_num(val,mwebmv) then $
			mwebmv = -99.
	endif else begin
		if prt then print,'Galactic E(B-V) not found'
		mwebmv = -99.
	endelse
;
; Get extinction in band
	if keyword_set(mwband) and n_elements(mwband) gt 0 then begin
		t=where(strpos(page,'Bandpass   ') ge 0, n)
		if n gt 0 then begin
			rec = page[t[0]]
			bp = gettok(rec,' ')
			p=0
			while strlen(rec) gt 0 do begin
				bp = gettok(rec,' ')
				p = p + 1
				if strpos(mwband,bp) ge 0 then break
			endwhile
			if strpos(bp,mwband) ge 0 then begin
				rec = page(t(0)+2)
				for i=0,p+1 do val=gettok(rec,' ')
				if not valid_num(val,mwalam) then $
					mwalam=-99.
			endif else begin
				if prt then print,'Bandpass not found: ',mwband
				mwalam=-99.
			endelse
		endif else begin
			if prt then print,'Bandpass list not found.'
			mwalam= -99.
		endelse
	endif
;
endif else print,'GET_NED - Wrong number of files found: ',nf,' ',obj
;
return
end
