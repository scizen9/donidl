;+
; VS_POSMATCH
; see if we have a position match on the visit server
;
; status = vs_posmatch(ra,dec,srch_rad_asec)
;-

function vs_posmatch, ra0, dec0, rad, DBCONN=dbconn, DBSTR=dbstr, $
	verbose=verbose, silent=silent, grelease=grelease, $
	eclipse=eclipse, fieldname=fieldname, exptime=exptime, utdate=utdate, $
	fuvmag=fuvmag, fuverr=fuverr, nuvmag=nuvmag, nuverr=nuverr, $
	ra=ra, dec=dec, distance=distance, nogi=nogi

  compile_opt idl2

  status = 1	; default status
  
  ; Check inputs
  if n_params(0) lt 2 then begin
	  print,'VS_POSMATCH: Usage - status = vs_posmatch( ra_deg, dec_deg, srchrad_asec, DBCONN=dbconn, DBSTR=dbstr, /verbose, /silent, /nogi)'
	  print,'Data items available through keywords:'
	  print,'grelease, eclipse, fieldname, exptime, utdate, fuvmag, fuverr, nuvmag, nuverr, ra, dec, distance'
	  return,status
  endif
  if n_params(0) lt 3 then rad = 4.0d0

  ; Did we pass a PgSql connection object?
  if not keyword_set(dbconn) then begin

  ; Default database connectioon string
    if n_elements(dbstr) lt 1 then $
	  dbconn = vs_conn() $
    else  dbconn = vs_conn(DBSTR=dbstr[0])
  
  endif
  
  ; Create an PgSql command object
  cmd = obj_new("pgSqlCmd", dbconn)
  
  ; Execute the command
  qry = "SELECT s.eclipse,s.fieldname,s.exptime,"+ $
	  "timezone('UTC',date_trunc('second',m.nexpstar)) AS utdate,"+ $
	  "CASE WHEN c.fuv_mag_auto < -90 THEN c.fuv_mag_auto ELSE "+ $
	  "c.fuv_mag_auto+18.82 END AS fuvmagauto,"+ $
	  "c.fuv_magerr_auto AS fuverrauto, "+ $
	  "CASE when c.nuv_mag_auto < -90 THEN c.nuv_mag_auto ELSE "+ $
	  "c.nuv_mag_auto+20.08 END AS nuvmagauto,"+ $
	  "c.nuv_magerr_auto AS nuverrauto,"+ $
	  "CASE WHEN c.fuv_mag_aper_1 < -90 THEN c.fuv_mag_aper_1 ELSE "+ $
	  "c.fuv_mag_aper_1+18.82 END AS fuvmag1,"+ $
	  "c.fuv_magerr_aper_1 AS fuverr1, "+ $
	  "CASE when c.nuv_mag_aper_1 < -90 THEN c.nuv_mag_aper_1 ELSE "+ $
	  "c.nuv_mag_aper_1+20.08 END AS nuvmag1,"+ $
	  "c.nuv_magerr_aper_1 AS nuverr1,"+ $
	  "CASE WHEN c.fuv_mag_aper_2 < -90 THEN c.fuv_mag_aper_2 ELSE "+ $
	  "c.fuv_mag_aper_2+18.82 END AS fuvmag2,"+ $
	  "c.fuv_magerr_aper_2 AS fuverr2, "+ $
	  "CASE when c.nuv_mag_aper_2 < -90 THEN c.nuv_mag_aper_2 ELSE "+ $
	  "c.nuv_mag_aper_2+20.08 END AS nuvmag2,"+ $
	  "c.nuv_magerr_aper_2 AS nuverr2,"+ $
	  "CASE WHEN c.fuv_mag_aper_3 < -90 THEN c.fuv_mag_aper_3 ELSE "+ $
	  "c.fuv_mag_aper_3+18.82 END AS fuvmag3,"+ $
	  "c.fuv_magerr_aper_3 AS fuverr3, "+ $
	  "CASE when c.nuv_mag_aper_3 < -90 THEN c.nuv_mag_aper_3 ELSE "+ $
	  "c.nuv_mag_aper_3+20.08 END AS nuvmag3,"+ $
	  "c.nuv_magerr_aper_3 AS nuverr3,"+ $
	  "CASE WHEN c.fuv_mag_aper_4 < -90 THEN c.fuv_mag_aper_4 ELSE "+ $
	  "c.fuv_mag_aper_4+18.82 END AS fuvmag4,"+ $
	  "c.fuv_magerr_aper_4 AS fuverr4, "+ $
	  "CASE when c.nuv_mag_aper_4 < -90 THEN c.nuv_mag_aper_4 ELSE "+ $
	  "c.nuv_mag_aper_4+20.08 END AS nuvmag4,"+ $
	  "c.nuv_magerr_aper_4 AS nuverr4,"+ $
	  "CASE WHEN c.fuv_mag_aper_5 < -90 THEN c.fuv_mag_aper_5 ELSE "+ $
	  "c.fuv_mag_aper_5+18.82 END AS fuvmag5,"+ $
	  "c.fuv_magerr_aper_5 AS fuverr5, "+ $
	  "CASE when c.nuv_mag_aper_5 < -90 THEN c.nuv_mag_aper_5 ELSE "+ $
	  "c.nuv_mag_aper_5+20.08 END AS nuvmag5,"+ $
	  "c.nuv_magerr_aper_5 AS nuverr5,"+ $
	  "CASE WHEN c.fuv_mag_aper_6 < -90 THEN c.fuv_mag_aper_6 ELSE "+ $
	  "c.fuv_mag_aper_6+18.82 END AS fuvmag6,"+ $
	  "c.fuv_magerr_aper_6 AS fuverr6, "+ $
	  "CASE when c.nuv_mag_aper_6 < -90 THEN c.nuv_mag_aper_6 ELSE "+ $
	  "c.nuv_mag_aper_6+20.08 END AS nuvmag6,"+ $
	  "c.nuv_magerr_aper_6 AS nuverr6,"+ $
	  "CASE WHEN c.fuv_mag_aper_7 < -90 THEN c.fuv_mag_aper_7 ELSE "+ $
	  "c.fuv_mag_aper_7+18.82 END AS fuvmag7,"+ $
	  "c.fuv_magerr_aper_7 AS fuverr7, "+ $
	  "CASE when c.nuv_mag_aper_7 < -90 THEN c.nuv_mag_aper_7 ELSE "+ $
	  "c.nuv_mag_aper_7+20.08 END AS nuvmag7,"+ $
	  "c.nuv_magerr_aper_7 AS nuverr7,"+ $
	  "todeg(long(c.plc)) as ra,"+ $
	  "todeg(lat(c.plc)) as dec,"+ $
	  '648000.0/pi()*(c.plc<->torad('+ $
	  strtrim(string(ra0,form='(f16.12)'),2)+','+ $
	  strtrim(string(dec0,form='(f16.12)'),2)+")) AS distance,"+ $
	  "s.grelease "+ $
  	  "FROM status s left outer join mcathdr m on m.stat=s.key "+ $
	  "left outer join catalog c on s.key=c.stat and c.plc @ "+ $
	  "scircle(torad("+ $
	  strtrim(string(ra0,form='(f16.12)'),2)+','+ $
	  strtrim(string(dec0,form='(f16.12)'),2)+ $
	  "),0.000004848136811095*"+strtrim(string(rad),2)+") "
  if keyword_set(nogi) then $
	  qry = qry + "and lower(substring(s.fieldname from 1 for 2)) <> 'gi' "
  qry = qry + $
	  "where s.exptime > 0 and s.qa_grade is distinct from 'FAIL' and " + $
	  "s.qa_coadd is distinct from 'NO' "+ $
	  "and s.ow='d' and s.cent @ scircle(torad("+ $
	  strtrim(string(ra0,form='(f16.12)'),2)+','+ $
	  strtrim(string(dec0,form='(f16.12)'),2)+ $
	  "),0.010471975511966) order by s.ecl_start, s.sub_visit"
  if keyword_set(verbose) then print,qry
  n = cmd -> Execute(qry,/verbose)

  if n lt 1 then begin
    obj_destroy, cmd
    if not keyword_set(dbconn) then obj_destroy, dbconn
    if keyword_set(verbose) and not keyword_set(silent) then $
	    print, 'No rows returned from query'
    return,status
  endif
  
  ; Get the results
  eclipse = cmd->GetField('eclipse')
  fieldname=cmd->GetField('fieldname')
  exptime = cmd->GetField('exptime')
  utdate  = cmd->GetField('utdate')
  fuvmagauto = cmd->GetField('fuvmagauto')
  fuverrauto = cmd->GetField('fuverrauto')
  nuvmagauto = cmd->GetField('nuvmagauto')
  nuverrauto = cmd->GetField('nuverrauto')
  fuvmag1 = cmd->GetField('fuvmag1')
  fuverr1 = cmd->GetField('fuverr1')
  nuvmag1 = cmd->GetField('nuvmag1')
  nuverr1 = cmd->GetField('nuverr1')
  fuvmag2 = cmd->GetField('fuvmag2')
  fuverr2 = cmd->GetField('fuverr2')
  nuvmag2 = cmd->GetField('nuvmag2')
  nuverr2 = cmd->GetField('nuverr2')
  fuvmag3 = cmd->GetField('fuvmag3')
  fuverr3 = cmd->GetField('fuverr3')
  nuvmag3 = cmd->GetField('nuvmag3')
  nuverr3 = cmd->GetField('nuverr3')
  fuvmag4 = cmd->GetField('fuvmag4')
  fuverr4 = cmd->GetField('fuverr4')
  nuvmag4 = cmd->GetField('nuvmag4')
  nuverr4 = cmd->GetField('nuverr4')
  fuvmag5 = cmd->GetField('fuvmag5')
  fuverr5 = cmd->GetField('fuverr5')
  nuvmag5 = cmd->GetField('nuvmag5')
  nuverr5 = cmd->GetField('nuverr5')
  fuvmag6 = cmd->GetField('fuvmag6')
  fuverr6 = cmd->GetField('fuverr6')
  nuvmag6 = cmd->GetField('nuvmag6')
  nuverr6 = cmd->GetField('nuverr6')
  fuvmag7 = cmd->GetField('fuvmag7')
  fuverr7 = cmd->GetField('fuverr7')
  nuvmag7 = cmd->GetField('nuvmag7')
  nuverr7 = cmd->GetField('nuverr7')
  ra      = cmd->GetField('ra')
  dec     = cmd->GetField('dec')
  distance= cmd->GetField('distance')
  grelease= cmd->GetField('grelease')
  status = 0

  ; Check against GI release database
  if not keyword_set(nogi) then begin
	use=intarr(n) + 1
	for i=0,n-1 do begin
	    if strpos(fieldname[i],'GI') ge 0 then begin
		p=strpos(fieldname[i],'_',/reverse_search)
		tile = strmid(fieldname[i],0,p)
		vis = fix(strmid(fieldname[i],p+1))
		use[i] = gx_gipublic(tile,vis)
	    endif
	endfor
	t=where(use eq 1, nt)
	if nt gt 0 then begin
		eclipse		= eclipse[t]
		fieldname	= fieldname[t]
		exptime		= exptime[t]
		utdate		= utdate[t]
		fuvmagauto	= fuvmagauto[t]
		fuverrauto	= fuverrauto[t]
		nuvmagauto	= nuvmagauto[t]
		nuverrauto	= nuverrauto[t]
		fuvmag1		= fuvmag1[t]
		fuverr1		= fuverr1[t]
		nuvmag1		= nuvmag1[t]
		nuverr1		= nuverr1[t]
		fuvmag2		= fuvmag2[t]
		fuverr2		= fuverr2[t]
		nuvmag2		= nuvmag2[t]
		nuverr2		= nuverr2[t]
		fuvmag3		= fuvmag3[t]
		fuverr3		= fuverr3[t]
		nuvmag3		= nuvmag3[t]
		nuverr3		= nuverr3[t]
		fuvmag4		= fuvmag4[t]
		fuverr4		= fuverr4[t]
		nuvmag4		= nuvmag4[t]
		nuverr4		= nuverr4[t]
		fuvmag5		= fuvmag5[t]
		fuverr5		= fuverr5[t]
		nuvmag5		= nuvmag5[t]
		nuverr5		= nuverr5[t]
		fuvmag6		= fuvmag6[t]
		fuverr6		= fuverr6[t]
		nuvmag6		= nuvmag6[t]
		nuverr6		= nuverr6[t]
		fuvmag7		= fuvmag7[t]
		fuverr7		= fuverr7[t]
		nuvmag7		= nuvmag7[t]
		nuverr7		= nuverr7[t]
		ra		= ra[t]
		dec		= dec[t]
		distance	= distance[t]
		grelease	= grelease[t]
		n = nt
	endif else begin
		status=1
		if keyword_set(verbose) and not keyword_set(silent) then $
			print,'No public rows returned'
		return,status
	endelse
  endif

  ; Combine the magnitudes
  fuvmag = [[fuvmagauto],[fuvmag1],[fuvmag2],[fuvmag3],[fuvmag4], $
	    [fuvmag5],[fuvmag6],[fuvmag7]]
  fuverr = [[fuverrauto],[fuverr1],[fuverr2],[fuverr3],[fuverr4], $
	    [fuverr5],[fuverr6],[fuverr7]]
  nuvmag = [[nuvmagauto],[nuvmag1],[nuvmag2],[nuvmag3],[nuvmag4], $
	    [nuvmag5],[nuvmag6],[nuvmag7]]
  nuverr = [[nuverrauto],[nuverr1],[nuverr2],[nuverr3],[nuverr4], $
	    [nuverr5],[nuverr6],[nuverr7]]

  ; Print the results
  if keyword_set(verbose) and not keyword_set(silent) then $
  	for i=0,n-1 do print,eclipse[i],fieldname[i],exptime[i],utdate[i], $
		fuvmag4[i],fuverr4[i],nuvmag4[i],nuverr4[i],ra[i],dec[i], $
		distance[i],grelease[i], $
		format='(i9,2x,a-32,f10.1,2x,a-25,4f8.3,2f13.8,f7.2,2x,a-16)'

  ; Destroy the objects
  obj_destroy, cmd
  if not keyword_set(dbconn) then obj_destroy, dbconn

  return,status

end
