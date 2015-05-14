pro sndatstat,list
;
; What data do we have for the input list of SNe ('YYYYA' or 'YYYYaa')
;
; get sn data
COMMON lowz_sne_info
;
; check input
if n_params(0) lt 1 then begin
	ilst=''
	read,'Input list of SNe: ',ilst
endif else ilst = list
;
; get list of sne
readcol,ilst,sne,format='a',skip=1
nsn=n_elements(sne)
;
; Open output file
ofl = strmid(ilst,0,strpos(ilst,'.')) + '.dstat'
filestamp,ofl
openw,ol,ofl,/get_lun
printf,ol,'#SNDATSTAT: run on '+systime(0)
printf,ol,'# input list: '+ilst
printf,ol,'#'
;
;  UV data status
printf,ol,'#GALEX data'
printf,ol,'#SN         Type      Host                  cz    FUV      NUV           .5k:   FUV      NUV           1.k:   FUV      NUV           2.k:   FUV      NUV'
for i=0,nsn-1 do begin
	s=snfind(strtrim(sne(i),2))
	if s gt -1 then begin
		printf,ol,sne(i), sndat(s).type, sndat(s).host, sndat(s).cz, $
			sndat(s).hfuv_int_mag, sndat(s).hnuv_int_mag, $
			sndat(s).ap_500pc, $
			sndat(s).hfuv_500pc_mag, sndat(s).hnuv_500pc_mag, $
			sndat(s).ap_1kpc, $
			sndat(s).hfuv_1kpc_mag, sndat(s).hnuv_1kpc_mag, $
			sndat(s).ap_2kpc, $
			sndat(s).hfuv_2kpc_mag, sndat(s).hnuv_2kpc_mag, $
	format='(a-10,2x,a-8,2x,a-16,2x,f7.1,2f9.3,f12.1,2f9.3,f12.1,2f9.3,f12.1,2f9.3)'
	endif
endfor
printf,ol,'#'
;
;  SDSS data status
printf,ol,'#SDSS data'
printf,ol,'#SN         Type      Host            int: u        g        r        i        z      .5k: u        g        r        i        z      1.k: u        g        r        i        z      2.k: u        g        r        i        z'
for i=0,nsn-1 do begin
	s=snfind(strtrim(sne(i),2))
	if s gt -1 then begin
		printf,ol,sne(i), sndat(s).type, sndat(s).host, $
			sndat(s).hu_int_mag, sndat(s).hg_int_mag, $
			sndat(s).hr_int_mag, sndat(s).hi_int_mag, $
			sndat(s).hz_int_mag, $
			sndat(s).hu_500pc_mag, sndat(s).hg_500pc_mag, $
			sndat(s).hr_500pc_mag, sndat(s).hi_500pc_mag, $
			sndat(s).hz_500pc_mag, $
			sndat(s).hu_1kpc_mag, sndat(s).hg_1kpc_mag, $
			sndat(s).hr_1kpc_mag, sndat(s).hi_1kpc_mag, $
			sndat(s).hz_1kpc_mag, $
			sndat(s).hu_2kpc_mag, sndat(s).hg_2kpc_mag, $
			sndat(s).hr_2kpc_mag, sndat(s).hi_2kpc_mag, $
			sndat(s).hz_2kpc_mag, $
   format='(a-10,2x,a-8,2x,a-16,2x,5f9.3,f12.3,4f9.3,f12.3,4f9.3,f12.3,4f9.3)'
	endif
endfor
printf,ol,'#'
;
;  2MASS data status
printf,ol,'#2MASS data'
printf,ol,'#SN         Type      Host            int: J        H        K      .5k: J        H        K      1.k: J        H        K      2.k: J        H        K'
for i=0,nsn-1 do begin
	s=snfind(strtrim(sne(i),2))
	if s gt -1 then begin
		printf,ol,sne(i), sndat(s).type, sndat(s).host, $
			sndat(s).hJ_int_mag, sndat(s).hH_int_mag, $
			sndat(s).hK_int_mag, $
			sndat(s).hJ_500pc_mag, sndat(s).hH_500pc_mag, $
			sndat(s).hK_500pc_mag, $
			sndat(s).hJ_1kpc_mag, sndat(s).hH_1kpc_mag, $
			sndat(s).hK_1kpc_mag, $
			sndat(s).hJ_2kpc_mag, sndat(s).hH_2kpc_mag, $
			sndat(s).hK_2kpc_mag, $
   format='(a-10,2x,a-8,2x,a-16,2x,3f9.3,f12.3,2f9.3,f12.3,2f9.3,f12.3,2f9.3)'
	endif
endfor
printf,ol,'#'
;
;  RC3 data status
printf,ol,'#RC3 data'
printf,ol,'#SN         Type      Host            int: U        B        V'
for i=0,nsn-1 do begin
	s=snfind(strtrim(sne(i),2))
	if s gt -1 then begin
		printf,ol,sne(i), sndat(s).type, sndat(s).host, $
			sndat(s).huJ_int_mag, sndat(s).hbJ_int_mag, $
			sndat(s).hvJ_int_mag, $
			format='(a-10,2x,a-8,2x,a-16,2x,3f9.3)'
	endif
endfor
printf,ol,'#'
;
;  IRAS data status
printf,ol,'#IRAS data'
printf,ol,'#SN         Type      Host            int: 12m      25m      60m    100m'
for i=0,nsn-1 do begin
	s=snfind(strtrim(sne(i),2))
	if s gt -1 then begin
		printf,ol,sne(i), sndat(s).type, sndat(s).host, $
			sndat(s).h12m_int_mag, sndat(s).h25m_int_mag, $
			sndat(s).h60m_int_mag, sndat(s).h100m_int_mag, $
			format='(a-10,2x,a-8,2x,a-16,2x,4f9.3)'
	endif
endfor
printf,ol,'#'
;
free_lun,ol
;
return
end
