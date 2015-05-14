;+
; VS_INFIELD
; see what fields contain given position
;
; status = vs_infield(ra,dec,rad_lim_deg)
;-

function vs_infield, ra0, dec0, rad, DBCONN=dbconn, DBSTR=dbstr, $
	verbose=verbose, silent=silent, grelease=grelease, $
	eclipse=eclipse, fieldname=fieldname, exptime=exptime, utdate=utdate, $
	info_str=info_str, ra=ra, dec=dec, distance=distance, nogi=nogi

  compile_opt idl2

  status = 1	; default status
  
  ; Check inputs
  if n_params(0) lt 2 then begin
	  print,'VS_INFIELD: Usage - status = vs_infield( ra_deg, dec_deg, rad_lim_deg, DBCONN=dbconn, DBSTR=dbstr, /verbose, /silent, /nogi)'
	  print,'Data items available through keywords:'
	  print,'grelease, eclipse, fieldname, exptime, utdate, info_str, ra, dec, distance'
	  return,status
  endif
  if n_params(0) lt 3 then rad = 0.5

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
	  "s.info_str,"+ $
	  "todeg(long(s.cent)) as ra,"+ $
	  "todeg(lat(s.cent)) as dec,"+ $
	  '180.0/pi()*(s.cent<->torad('+ $
	  strtrim(string(ra0,form='(f16.12)'),2)+','+ $
	  strtrim(string(dec0,form='(f16.12)'),2)+")) AS distance,"+ $
	  "s.grelease "+ $
  	  "FROM status s left outer join mcathdr m on m.stat=s.key "
  if keyword_set(nogi) then $
	  qry = qry + "and lower(substring(s.fieldname from 1 for 2)) <> 'gi' "
  qry = qry + $
	  "where s.exptime > 0 and s.qa_grade is distinct from 'FAIL' and " + $
	  "s.qa_coadd is distinct from 'NO' "+ $
	  "and s.ow='d' and s.cent @ scircle(torad(" + $
	  strtrim(string(ra0,form='(f16.12)'),2)+','+ $
	  strtrim(string(dec0,form='(f16.12)'),2)+ $
	  "),0.017453293*"+strtrim(string(rad),2)+") " + $
	  "order by s.ecl_start, s.sub_visit"
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
  info_str= cmd->GetField('info_str')
  ra      = cmd->GetField('ra')
  dec     = cmd->GetField('dec')
  distance= cmd->GetField('distance')
  grelease= cmd->GetField('grelease')
  status = 0

  ; Print the results
  if keyword_set(verbose) and not keyword_set(silent) then $
  	for i=0,n-1 do print,eclipse[i],fieldname[i],exptime[i],utdate[i], $
		ra[i],dec[i],distance[i],grelease[i],info_str[i], $
		format='(i9,2x,a-32,f10.1,2x,a-25,2f13.8,f7.2,2x,a-12,2x,a)'

  ; Destroy the objects
  obj_destroy, cmd
  if not keyword_set(dbconn) then obj_destroy, dbconn

  return,status

end
