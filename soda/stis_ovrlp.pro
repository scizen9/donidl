pro stis_ovrlp,fcsv
;
; compare overlap between STIS obs and mission status file
;
; open mission status file
ms=mrdfits('/Users/neill/galex/logs/mission_status.fits',1,msh)
;
; open csv database
readcol,fcsv,ds,targ,rastr,decstr,strt,exptim,ap,filt,prop,type, $
	format='a,a,a,a,a,f,a,a,l,a',delimiter=','
;readcol,fcsv,ds,targ,rastr,decstr,ref,strt,stp,exptim,instr,ap,filt,prop,junk, $
;	format='a,a,a,a,i,a,a,f,a,a,a,l,a',delimiter=','
;
; open match file
tmp=fcsv
rute=gettok(tmp,'.')
ofil=rute+'.match'
openw,ol,ofil,/get_lun
;
; loop over STIS obs
nstis=0L
ngalx=0L
n=n_elements(exptim)
for i=0,n-1 do begin
;
; check coord match
	radec_parse,rastr(i),decstr(i),' ',rad,decd
	gcirc,2,rad,decd,ms.ra0,ms.dec0,r	; distances in arcsec
	r = r/3600.d0				; convert to degrees
	t=where(r le 0.5, nf)			; get obs within field
	if nf gt 0 then begin
		nstis=nstis+1L
		r = r(t)
		; print STIS record
		printf,ol,targ(i),rad,decd,strt(i),exptim(i),ap(i),filt(i), $
		   prop(i), format='(a16,2x,2f10.5,2x,a26,2x,f10.2,a8,2x,a8,i6)'
		; print GALEX matchin obs
		for j=0,nf-1 do begin
			ngalx=ngalx+1L
			p=t(j)
			printf,ol,ms(p).eclipse,ms(p).tile,ms(p).ow, $
				ms(p).ra0,ms(p).dec0,ms(p).nuv_exptime, $
				ms(p).surv_type,ms(p).ecl_start, r(j), $
		    format='(i6,2x,a26,2x,a6,2x,2f10.5,f10.2,2x,a8,2x,a16,f6.2)'
		endfor
		printf,ol,' '

	endif
endfor	; loop over STIS obs
;
; close and exit
free_lun,ol
;
print,nstis,' STIS observations matched'
print,ngalx,' GALEX eclipses matched'
;
return
end
