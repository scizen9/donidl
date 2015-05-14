pro snobstat,lfile
;+
; snobstat - output a list of SNe with host photometry adequate for SED fits
;-
common lowz_sne_info
;
sam=get_snlist(lfile,nsam=nsam)
;
; open output file
temp = lfile
root = gettok(temp,'.')
ofil = root + '_obstat.txt'
filestamp,ofil
openw,ol,ofil,/get_lun
printf,ol,'# SNOBSTAT: '+systime(0)
printf,ol,'# Input list: '+lfile
printf,ol,'# Number SNe: '+string(nsam,format='(i6)')
printf,ol,'# SN, Delta, Str, HOST, NFitBands, UVBands, OpticalBands, NIRBands, FIRBands'
;
for i=0,nsam-1 do begin
	p = sam[i]
	nuvb = 0
	nopb = 0
	nnib = 0
	nfib = 0
	;
	; uv bands
	if sndat[p].hFUV_int_mag gt 0. and sndat[p].hFUV_int_magerr le 0.362 $
		and sndat[p].hFUV_int_magerr ge 0. then nuvb = nuvb + 1
	if sndat[p].hNUV_int_mag gt 0. and sndat[p].hNUV_int_magerr le 0.362 $
		and sndat[p].hNUV_int_magerr ge 0. then nuvb = nuvb + 1
	;
	; optical bands
	if sndat[p].huJ_int_mag gt 0. and sndat[p].huJ_int_magerr le 0.362 $
		and sndat[p].huJ_int_magerr ge 0. then nopb = nopb + 1
	if sndat[p].hbJ_int_mag gt 0. and sndat[p].hbJ_int_magerr le 0.362 $
		and sndat[p].hbJ_int_magerr ge 0. then nopb = nopb + 1
	if sndat[p].hvJ_int_mag gt 0. and sndat[p].hvJ_int_magerr le 0.362 $
		and sndat[p].hvJ_int_magerr ge 0. then nopb = nopb + 1
	if sndat[p].hu_int_mag gt 0. and sndat[p].hu_int_magerr le 0.362 $
		and sndat[p].hu_int_magerr ge 0. then nopb = nopb + 1
	if sndat[p].hg_int_mag gt 0. and sndat[p].hg_int_magerr le 0.362 $
		and sndat[p].hg_int_magerr ge 0. then nopb = nopb + 1
	if sndat[p].hr_int_mag gt 0. and sndat[p].hr_int_magerr le 0.362 $
		and sndat[p].hr_int_magerr ge 0. then nopb = nopb + 1
	if sndat[p].hi_int_mag gt 0. and sndat[p].hi_int_magerr le 0.362 $
		and sndat[p].hi_int_magerr ge 0. then nopb = nopb + 1
	if sndat[p].hz_int_mag gt 0. and sndat[p].hz_int_magerr le 0.362 $
		and sndat[p].hz_int_magerr ge 0. then nopb = nopb + 1
	;
	; NIR bands
	if sndat[p].hJ_int_mag gt 0. and sndat[p].hJ_int_magerr le 0.362 $
		and sndat[p].hJ_int_magerr ge 0. then nnib = nnib + 1
	if sndat[p].hH_int_mag gt 0. and sndat[p].hH_int_magerr le 0.362 $
		and sndat[p].hH_int_magerr ge 0. then nnib = nnib + 1
	if sndat[p].hK_int_mag gt 0. and sndat[p].hK_int_magerr le 0.362 $
		and sndat[p].hK_int_magerr ge 0. then nnib = nnib + 1
	;
	; FIR bands
	if sndat[p].h12m_int_mag gt 0. and sndat[p].h12m_int_magerr le 0.362 $
		and sndat[p].h12m_int_magerr ge 0. then nfib = nfib + 1
	if sndat[p].h25m_int_mag gt 0. and sndat[p].h25m_int_magerr le 0.362 $
		and sndat[p].h25m_int_magerr ge 0. then nfib = nfib + 1
	if sndat[p].h60m_int_mag gt 0. and sndat[p].h60m_int_magerr le 0.362 $
		and sndat[p].h60m_int_magerr ge 0. then nfib = nfib + 1
	if sndat[p].h100m_int_mag gt 0. and sndat[p].h100m_int_magerr le 0.362 $
		and sndat[p].h100m_int_magerr ge 0. then nfib = nfib + 1
	;
	; fit bands
	nfit = nuvb + nopb + nnib
	;
	; print results
	printf,ol,sndat[p].id, $
		sndat[p].cfa3_delta, $
		sndat[p].smpl_str, $
		sndat[p].host, $
		nfit, nuvb, nopb, nnib, nfib, $
		format='(a-10,2f7.2,2x,a-15,5i5)'
endfor
;
free_lun,ol
;
return
end
