pro get_ned_coord,obj, $
    eq_ra_b=eq_ra_b,eq_dec_b=eq_dec_b,eq_ra_j=eq_ra_j,eq_dec_j=eq_dec_j, $
    ec_lon_b=ec_lon_b,ec_lat_b=ec_lat_b,ec_lon_j=ec_lon_j,ec_lat_j=ec_lat_j, $
    gl_lon=gl_lon,gl_lat=gl_lat,sg_lon=sg_lon,sg_lat=sg_lat, $
    reread=reread,silent=silent
;
; see if it's in cache
nedir=!NED_CACHE
fspec = nedir+'/'+obj+'.html'
flist=file_search(fspec,count=nf)
;
; if not, use wget to retrieve NED data
if nf lt 1 or keyword_set(reread) then begin
    cmd='wget "http://nedwww.ipac.caltech.edu/cgi-bin/nph-objsearch?objname='+$
	    obj+' & extend=no & out_csys=Equatorial & out_equinox=J2000.0 & obj_sort=RA+or+Longitude & of=pre_text & zv_breaker=30000.0 & list_limit=5 & img_stamp=NO" -O '+nedir+'/'+obj+'.html'
    if not keyword_set(silent) then begin
	    print,'wgetting from NED...'
	    print,cmd
    endif
    spawn,cmd,res,eres
endif
;
; get file
flist=file_search(fspec,count=nf)
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
	free_lun,il
;
; check output
	if n_elements(page) lt 2 then begin
		print,obj+' file error, recommend using /reread.'
		page=['']
	endif else page=page[1:*]
;
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
; Equatorial (B1950.0)
	t=where(strpos(page,'Equatorial (B1950.0)') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,1 do val=gettok(rec,' ')
		eq_ra_b = double(gettok(rec,' '))
		eq_dec_b= double(gettok(rec,' '))
	endif else begin
		if prt and keyword_set(eq_ra_b) then print,'Equatorial (B1950.0) not found'
		eq_ra_b = -99.
		eq_dec_b= -99.
	endelse
;
; Equatorial (J2000.0)
	t=where(strpos(page,'Equatorial (J2000.0)') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,1 do val=gettok(rec,' ')
		eq_ra_j = double(gettok(rec,' '))
		eq_dec_j= double(gettok(rec,' '))
	endif else begin
		if prt and keyword_set(eq_ra_j) then print,'Equatorial (J2000.0) not found'
		eq_ra_j = -99.
		eq_dec_j= -99.
	endelse
;
; Ecliptic (B1950.0)
	t=where(strpos(page,'Ecliptic   (B1950.0)') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,1 do val=gettok(rec,' ')
		ec_lon_b = double(gettok(rec,' '))
		ec_lat_b = double(gettok(rec,' '))
	endif else begin
		if prt and keyword_set(ec_lon_b) then print,'Ecliptic (B1950.0) not found'
		ec_lon_b = -99.
		ec_lat_b = -99.
	endelse
;
; Ecliptic (J2000.0)
	t=where(strpos(page,'Ecliptic   (J2000.0)') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		for i=0,1 do val=gettok(rec,' ')
		ec_lon_j = double(gettok(rec,' '))
		ec_lat_j = double(gettok(rec,' '))
	endif else begin
		if prt and keyword_set(ec_lon_j) then print,'Ecliptic (J2000.0) not found'
		ec_lon_j = -99.
		ec_lat_j = -99.
	endelse
;
; Galactic
	t=where(strpos(page,'Galactic              ') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		val=gettok(rec,' ')
		gl_lon = double(gettok(rec,' '))
		gl_lat = double(gettok(rec,' '))
	endif else begin
		if prt and keyword_set(gl_lon) then print,'Galactic not found'
		gl_lon = -99.
		gl_lat = -99.
	endelse
;
; SuperGalactic
	t=where(strpos(page,'SuperGalactic         ') ge 0,n)
	if n gt 0 then begin
		rec = page[t[0]]
		val=gettok(rec,' ')
		sg_lon = double(gettok(rec,' '))
		sg_lat = double(gettok(rec,' '))
	endif else begin
		if prt and keyword_set(sg_lon) then print,'SuperGalactic not found'
		sg_lon = -99.
		sg_lat = -99.
	endelse

endif else print,'GET_NED_COORD - Wrong number of files found: ',nf,' ',obj
;
return
end
