pro mk_ais_sec_table, aissecfile=aissecfile, outfile=outfile
;
; uses fits versions of mission_status and ais_sec_gridmap
;
; aissecfile='~/mission-status/ais_sec_gridmap.fits.gz'
; outfile='mission_status_sg.fits'
;

if not keyword_set(outfile) then $
 outfile='ais_sec_table.sql'

if not keyword_set(aissecfile) then $
  aissecfile='/home/galex/fltops/mps/ref/ais_sec_gridmap.fits.gz'

;;;;;;;;;;;;;;;;;;;;;;;;;;
; read secondary tile file

ais2=mrdfits(aissecfile,1,/silent)
ais2.mps_target_id=strcompress(ais2.mps_target_id,/rem)
ais2.sec_target_id=strcompress(ais2.sec_target_id,/rem)

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

n=n_elements(ais2)

filestamp,outfile,/arch
openw,ol,outfile,/get_lun

for i=0l,n-1 do begin
	pref=pathnew[i]+'/'+basenew[i]
	cat = file_info(pref+'-xd-mcat.fits')
	fuv = file_info(pref+'-fd-rr.fits')
	nuv = file_info(pref+'-nd-rr.fits')
	ctim = (cat.exists) ? cat.mtime : -9999.0
	ftim = (fuv.exists) ? fuv.mtime : -9999.0
	ntim = (nuv.exists) ? nuv.mtime : -9999.0
	printf,ol,i+1,ais2[i].eclipse,ais2[i].plan_id,ais2[i].sky_grid, $
		ais2[i].sub_grid,ais2[i].sec_visit,ais2[i].mps_plan_id, $
		ais2[i].mps_target_id,ais2[i].mps_leg,ais2[i].mps_ra, $
		ais2[i].mps_dec,ais2[i].ais_grid,ais2[i].ais_grid_ra, $
		ais2[i].ais_grid_dec,ais2[i].offset_arcsec, $
		ais2[i].sec_target_id,basenew[i],pathnew[i],ctim,ftim,ntim, $
format='(%"%d\t%d\t%d\t%d\t%d\t%d\t%d\t%s\t%d\t%13.8f\t%13.8f\t%d\t%13.8f\t%13.8f\t%9.3f\t%s\t%s\t%s\t%12.1f\t%12.1f\t%12.1f")'
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; done
 
free_lun,ol
print,'Finis.'

end
