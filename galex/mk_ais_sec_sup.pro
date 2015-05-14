pro mk_ais_sec_sup, aissecfile=aissecfile, supfile=supfile, verbose=verbose
;+
;
; add supplemental information to ais_sec_gridmap
;
; aissecfile='~/mission-status/ais_sec_gridmap.fits.gz'
; supfile='ais_sec_gridmap_sup.fits'
;
; mk_ais_sec_sup, aissecfile, supfile, /verbose
;-

if not keyword_set(supfile) then $
 supfile='ais_sec_gridmap_sup.fits'

if not keyword_set(aissecfile) then begin
	if strpos(!version.os, 'linux') ge 0 then $
  		aissecfile='/users/neill/ref/galex/ais_sec_gridmap.fits.gz' $
	else	aissecfile='/srl/neill/ref/galex/ais_sec_gridmap.fits.gz'
endif

if supfile eq aissecfile then begin
 print,'Error with filenames. Exiting.'
 return
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;
; read secondary tile file

ais0=mrdfits(aissecfile,0,hdr0)
ais2=mrdfits(aissecfile,1,hdr1)
na = n_elements(ais2)

A = { eclipse:0L, plan_id:0L, sky_grid:0L, sub_grid:0L, $
	sec_visit:0L, mps_plan_id:0L, mps_target_id:'', mps_leg:0L, $
	mps_ra:0.d0, mps_dec:0.d0, ais_grid:0L, $
	ais_grid_ra:0.d0, ais_grid_dec:0.d0, offset_arcsec:0.d0, $
	sec_target_id:'', sec_base:'', pipedir:'', $
	grelease:'', exptime:0., fuv_exptime:0., nuv_exptime:0., $
	asp_ave_ra_rta:0.d0, asp_ave_dec_rta:0.d0, asp_ave_roll_rta:0.d0, $
	qa_vsn:0L, qa_id:'', qa_grade:'', qa_coadd:'', qa_date:'', $
	qa_flags:'', qa_manflags:'', qa_comments:'' }

aiss = replicate(A, na)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; define new values

tilenew = strcompress(ais2.sec_target_id,/rem)
visitnew = string(ais2.sec_visit,format='(i4.4)')
svnew = strcompress(string(ais2.sub_grid,format='(i2.2)'),/rem)
basenew = tilenew+'_'+visitnew+'_sg'+svnew
pathnew='/home/galex/fltops/visits/02-vsn/'+strcompress(ais2.plan_id,/rem)+$
 '-'+tilenew+'/d/00-visits/'+visitnew+'-img/use'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; do supplemental update

print,''
print,'Updating secondary grid supplemental info.'

for i=0L,na-1 do begin
	aiss[i].eclipse		= ais2[i].eclipse
	aiss[i].plan_id		= ais2[i].plan_id
	aiss[i].sky_grid	= ais2[i].sky_grid
	aiss[i].sub_grid	= ais2[i].sub_grid
	aiss[i].sec_visit	= ais2[i].sec_visit
	aiss[i].mps_plan_id	= ais2[i].mps_plan_id
	aiss[i].mps_target_id	= ais2[i].mps_target_id
	aiss[i].mps_leg		= ais2[i].mps_leg
	aiss[i].mps_ra		= ais2[i].mps_ra
	aiss[i].mps_dec		= ais2[i].mps_dec
	aiss[i].ais_grid	= ais2[i].ais_grid
	aiss[i].ais_grid_ra	= ais2[i].ais_grid_ra
	aiss[i].ais_grid_dec	= ais2[i].ais_grid_dec
	aiss[i].offset_arcsec	= ais2[i].offset_arcsec
	aiss[i].sec_target_id	= ais2[i].sec_target_id
	aiss[i].sec_base	= ais2[i].sec_base
	aiss[i].pipedir		= ais2[i].pipedir
	mcat = pathnew[i]+'/'+basenew[i]+'-xd-mcat.fits'
	if file_test(mcat) then begin
		mhdr = headfits(mcat)
		aiss[i].grelease = sxpar(mhdr,'grelease')
		aiss[i].exptime = sxpar(mhdr,'exptime')
		aiss[i].fuv_exptime = sxpar(mhdr,'fexptime')
		aiss[i].nuv_exptime = sxpar(mhdr,'nexptime')
		aiss[i].asp_ave_ra_rta = sxpar(mhdr,'avaspra')
		aiss[i].asp_ave_dec_rta = sxpar(mhdr,'avaspdec')
		aiss[i].asp_ave_roll_rta = sxpar(mhdr,'avasprol')
	endif else $
		if keyword_set(verbose) then print,'Not found: ',mcat
	qafil = pathnew[i]+'/qa/manual/'+basenew[i]+'-qa.txt'
	if file_test(qafil) then begin
		readcol,qafil,key,type,equals,value,form='a,a,a,a',/silent
		t=where(strpos(key,'QA:Vsn') ge 0, nt)
		if nt eq 1 then aiss[i].qa_vsn = long(value[t[0]])
		t=where(strpos(key,'QA:AnalystID') ge 0, nt)
		if nt eq 1 then if strcmp(value[t[0]],'NA') ne 1 then $
			aiss[i].qa_id = value[t[0]]
		t=where(strpos(key,'QA:Grade') ge 0, nt)
		if nt eq 1 then aiss[i].qa_grade = value[t[0]]
		t=where(strpos(key,'QA:Coadd') ge 0, nt)
		if nt eq 1 then aiss[i].qa_coadd = value[t[0]]
		t=where(strpos(key,'QA:Date') ge 0, nt)
		if nt eq 1 then if strcmp(value[t[0]],'NA') ne 1 then $
			aiss[i].qa_date = value[t[0]]
		t=where(strpos(key,'QA:Flags') ge 0, nt)
		if nt eq 1 then if strcmp(value[t[0]],'NA') ne 1 then $
			aiss[i].qa_flags = value[t[0]]
		t=where(strpos(key,'QA:CommentFlags') ge 0, nt)
		if nt eq 1 then if strcmp(value[t[0]],'NA') ne 1 then $
			aiss[i].qa_manflags = value[t[0]]
		t=where(strpos(key,'QA:Comments') ge 0, nt)
		if nt eq 1 then if strcmp(value[t[0]],'NA') ne 1 then $
			aiss[i].qa_comments = value[t[0]]
	endif else $
		if keyword_set(verbose) then print,'Not found: ',qafil
	if keyword_set(verbose) then $
		if i mod 100 eq 0 then print,i,'/',na,' ',basenew[i]
endfor

print,'Updates finished.'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; write file
 
print,''
print,'Writing file: '+supfile
filestamp,supfile,/arch
mwrfits,ais0, supfile,hdr0,/create
mwrfits,aiss, supfile,hdr

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; done
 
print,'Finis.'

end
