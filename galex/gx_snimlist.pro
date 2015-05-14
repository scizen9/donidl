pro gx_snimlist
;+
; GX_SNIMLIST - get list of GALEX images for each SN type
;-
;
common lowz_sne_info
;
; SN types
tylist=[ $
	'I', $
	'Ia', $
	'Ib', 'Ib_c', 'Ic', $
	'IIn', 'IIb', $
	'IIL', 'IIP', $
	'II' ]
nty = n_elements(tylist)
ntsn = 0L
;
; get mission status file
msf = '/home/galex/fltops/logs/mission_status/mission_status.fits'
if file_test(msf) then $
	ms = mrdfits(msf,1,msh) $
else	begin
	print,'Mission status file unavailable, try later.'
	return
endelse
;
; loop over types
for it=0,nty-1 do begin
	ty=tylist(it)
	tydir = '~/snsites/'+ty+'/'
;
; get sne of interest
    s=where(sndat.tyn eq it, nsn)
    if nsn le 0 then begin
	print,'No ',tylist(it),' SNe found: '
    endif else begin
;
; check for existing list
	ofile = tydir+ty+'_gximlist.txt'
	filestamp,ofile
	openw,ol,ofile,/get_lun
;
; loop over sne
	for j=0,nsn-1 do begin
		i=s(j)
;
; get coords
		ctyp=''
;
; first check sn coords
		if sndat(i).ra ge 0. and sndat(i).dec ge -90. then begin
			dra=sndat(i).ra
			ddec=sndat(i).dec
			ctyp='sn'
;
; if not check host coords
		endif else begin
;
; so we use the host coords
	        	if sndat(i).hra ge 0. and sndat(i).hdec ge -90. then $
				begin
				dra=sndat(i).hra
				ddec=sndat(i).hdec
				ctyp='host'
;
; host coords no good either
			endif else begin
				dra = -9.
				ddec= -99.
				ctyp='none'
			endelse
		endelse
;
; get galaxy size (minimum of arcmin)
		hsiz = sndat(i).hd25 > 60.
;
; get image list
		nim = 0L
		if dra ge 0. then begin
		    gx_mstat_tfind,ms,dra,ddec,hsiz,tiles,rads,vdirs,svis, $
			    rlim=0.60
		    if strtrim(tiles(0),2) ne '' then nim = n_elements(tiles)
		endif
		f='SN'
	fmt='(a-4,a-8,2f13.8,1x,a-8,1x,a-14,2f13.8,2f8.1,1x,a-8,f5.1,f11.3,i5)'
		printf,ol,f,sndat(i).id,sndat(i).ra,sndat(i).dec,sndat(i).type,$
			sndat(i).host,sndat(i).hra,sndat(i).hdec, $
			sndat(i).hd25,sndat(i).hinc, sndat(i).htype, $
			sndat(i).htyn, sndat(i).cz,nim,format=fmt
		print,f,sndat(i).id,sndat(i).ra,sndat(i).dec,sndat(i).type, $
			sndat(i).host,sndat(i).hra,sndat(i).hdec, $
			sndat(i).hd25,sndat(i).hinc, sndat(i).htype, $
			sndat(i).htyn, sndat(i).cz,nim,format=fmt
		if nim gt 0 then $
			for k=0,nim-1 do begin
				printf,ol,rads(k),svis(k),tiles(k),vdirs(k), $
					format='(f7.3,i5,2x,a-36,a)'
				print,rads(k),svis(k),tiles(k),vdirs(k), $
					format='(f7.3,i5,2x,a-36,a)'
			endfor
		printf,ol,' '
		print,' '
	endfor
	free_lun,ol
;
	print,'Found: ',nsn,' SNe type '+ty
	ntsn = ntsn + long(nsn)
    endelse	; some SNe were found
endfor	; loop over types
print,'Found: ',ntsn,' Total SNe of all types'
;
return
end
