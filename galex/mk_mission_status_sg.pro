pro mk_mission_status_sg, missionstatfile=missionstatfile, $
	aissecfile=aissecfile, outfile=outfile, logfile=logfile, $
	verbose=verbose

;ex: 
;
; uses fits versions of mission_status and ais_sec_gridmap
;
; missionstatfile='~/mission-status/mission_status.fits'
; aissecfile='~/mission-status/ais_sec_gridmap.fits.gz'
; outfile='mission_status_sg.fits'
;
; mk_mission_status_sg, missionstatfile, aissecfile, outfile

if not keyword_set(outfile) then $
 outfile=!GLGA_ROOT+'work/mission_status_sg.fits'

if not keyword_set(missionstatfile) then $
  missionstatfile='/home/galex/fltops/logs/mission_status/mission_status.fits'

if not keyword_set(aissecfile) then begin
	if strpos(!version.os,'linux') ge 0 then $
  		aissecfile='/users/neill/ref/galex/ais_sec_gridmap_sup.fits.gz'$
  	else	aissecfile='/srl/neill/ref/galex/ais_sec_gridmap_sup.fits.gz'
endif

if outfile eq aissecfile or outfile eq missionstatfile or $
 missionstatfile eq  aissecfile then begin
 print,'Error with filenames. Exiting.'
 return
endif


;;;;;;;;;;;;;;;;;;;;;;;;;;
; read mission_status file

g0=mrdfits(missionstatfile,0,hdr0)
g=mrdfits(missionstatfile,1,hdr)
g.tile=strcompress(g.tile,/rem)
g.info_str=strcompress(g.info_str,/rem)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; read secondary tile file

ais2=mrdfits(aissecfile,1,/silent)
ais2.mps_target_id=strcompress(ais2.mps_target_id,/rem)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; define new values

tilenew = strcompress(ais2.SEC_target_id,/rem)
visitnew = string(ais2.sec_visit,format='(i4.4)')
svnew = strcompress(string(ais2.sub_grid,format='(i2.2)'),/rem)
basenew = tilenew+'_'+visitnew+'_sg'+svnew
pathnew='/home/galex/fltops/visits/02-vsn/'+strcompress(ais2.plan_id,/rem)+$
 '-'+tilenew+'/d/00-visits/'+visitnew+'-img/use'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; parameters for quick match

m=lonarr(n_elements(ais2))

id1=g.tile+'_'+strcompress(string(g.leg),/rem)+'_'+$
 strcompress(string(g.eclipse),/rem)
id2=ais2.mps_target_id+'_'+strcompress(string(ais2.mps_leg),/rem)+'_'+$
 strcompress(string(ais2.eclipse),/rem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; do matching


print,''
print,'Matching secondary to mission_status (<3 mins). ',systime()
for i=0l,n_elements(ais2)-1 do m[i]=where(id1 eq id2[i])
print,'Matching finished. ',systime()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; do update


print,''
print,'Updating mission_status info.'

a=where(m ge 0)
g[m[a]].info_str=pathnew[a]
g[m[a]].base=basenew[a]
g[m[a]].tile=tilenew[a]
g[m[a]].visit=long(visitnew[a])
g[m[a]].sub_visit=long(svnew[a])
g[m[a]].grelease=ais2[a].grelease
g[m[a]].exptime=ais2[a].exptime
g[m[a]].fuv_exptime=ais2[a].fuv_exptime
g[m[a]].nuv_exptime=ais2[a].nuv_exptime
g[m[a]].asp_ave_ra_rta=ais2[a].asp_ave_ra_rta
g[m[a]].asp_ave_dec_rta=ais2[a].asp_ave_dec_rta
g[m[a]].asp_ave_roll_rta=ais2[a].asp_ave_roll_rta
g[m[a]].qa_vsn=ais2[a].qa_vsn
g[m[a]].qa_id=ais2[a].qa_id
g[m[a]].qa_grade=ais2[a].qa_grade
g[m[a]].qa_coadd=ais2[a].qa_coadd
g[m[a]].qa_date=ais2[a].qa_date
g[m[a]].qa_flags=ais2[a].qa_flags
g[m[a]].qa_manflags=ais2[a].qa_manflags
g[m[a]].comments=ais2[a].qa_comments

print,'Updates finished.'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; write file
 
print,''
print,'Writing file: '+outfile
mwrfits,g0, outfile,hdr0,/create
mwrfits,g, outfile,hdr

;
; log if requested
if keyword_set(logfile) then begin
	lfile = 'mission_status_sg.log'
	filestamp,lfile,/arch
	openw,ol,lfile,/get_lun
	printf,ol,'#MK_MISSION_STATUS_SG: '+systime(0)
	printf,ol,'#UNMATCHED SECONDARY TILES'
	printf,ol,'#SEC_BASE            MPS_TARG           MLEG  ECLIPSE'
	b = where(m le 0, nb)
	for i=0,nb-1 do begin
		p=b[i]
		printf,ol,basenew[p],ais2[p].mps_target_id,ais2[p].mps_leg, $
			ais2[p].eclipse, format='(a17,4x,a17,i6,i9)'
	endfor
	free_lun,ol
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; done
 
print,'Finis.'

end
