pro lephare_galdat_cat,t,samnam,saminfo,testmags=testmags,context=context
;
; print out the photometry table:
;
common galdb_info
;
if n_params(0) lt 3 then begin
	print,'Usage: lephare_galdat_cat, <indx_list>, <samnam>, <saminfo>'
	return
endif
;
; get directory
odir = timestr()
file_mkdir, odir
;
; CONTEXT
if keyword_set(context) then $
	cntxt = long(context) $
else	cntxt = 1023L		; default to all UV/Optical bands
;
; get sample
galsam=galdat(t)
nsam=n_elements(t)
;
; limit errors
x=where(galsam.FUV_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).FUV_int_magerr = galsam(x).FUV_int_magerr>0.01
x=where(galsam.NUV_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).NUV_int_magerr = galsam(x).NUV_int_magerr>0.01
x=where(galsam.uJ_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).uJ_int_magerr = galsam(x).uJ_int_magerr>0.01
x=where(galsam.bJ_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).bJ_int_magerr = galsam(x).bJ_int_magerr>0.01
x=where(galsam.vJ_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).vJ_int_magerr = galsam(x).vJ_int_magerr>0.01
x=where(galsam.u_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).u_int_magerr = galsam(x).u_int_magerr>0.01
x=where(galsam.g_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).g_int_magerr = galsam(x).g_int_magerr>0.01
x=where(galsam.r_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).r_int_magerr = galsam(x).r_int_magerr>0.01
x=where(galsam.i_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).i_int_magerr = galsam(x).i_int_magerr>0.01
x=where(galsam.z_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).z_int_magerr = galsam(x).z_int_magerr>0.01
x=where(galsam.J_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).J_int_magerr = galsam(x).J_int_magerr>0.01
x=where(galsam.H_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).H_int_magerr = galsam(x).H_int_magerr>0.01
x=where(galsam.K_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).K_int_magerr = galsam(x).K_int_magerr>0.01
x=where(galsam.w1_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).w1_int_magerr = galsam(x).w1_int_magerr>0.01
x=where(galsam.w2_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).w2_int_magerr = galsam(x).w2_int_magerr>0.01
x=where(galsam.w3_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).w3_int_magerr = galsam(x).w3_int_magerr>0.01
x=where(galsam.w4_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).w4_int_magerr = galsam(x).w4_int_magerr>0.01
x=where(galsam.i12m_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).i12m_int_magerr = galsam(x).i12m_int_magerr>0.01
x=where(galsam.i25m_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).i25m_int_magerr = galsam(x).i25m_int_magerr>0.01
x=where(galsam.i60m_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).i60m_int_magerr = galsam(x).i60m_int_magerr>0.01
x=where(galsam.i100m_int_magerr ge 0., nx)
if nx gt 0 then galsam(x).i100m_int_magerr = galsam(x).i100m_int_magerr>0.01
;
; open output file
ofil=odir+'/lephare_'+odir+'.mags'
openw,li,ofil,/get_lun
printf,li,'# LEPHARE_GALDAT_CAT: '+systime(0)
if keyword_set(testmags) then begin
	ntst = testmags
	printf,li,'# Testing Mags: at least '+strn(ntst)+' must be >= 0'
endif	else	begin
	ntst = 0
	printf,li,'# NOT Testing Mags'
endelse
printf,li,'# '+saminfo
printf,li,'#'

fmt='(i-8,42f9.2,i8,f7.3,2x,a-25,f9.1,f7.3,2x,a)'
;
; header
hdr='# ID         FUV      FUVe     NUV      NUVe      U        Ue       B        Be       V        Ve    u_sdss     usde   g_sdss     gsde   r_sdss     rsde   i_sdss     isde   z_sdss     zsde      J        Je       H        He       K        Ke       W1       W1e      W2       W2e      W3       W3e      W4       W4e     12mu    12mue     25mu    25mue     60mu    60mue    100mu   100mue   cntxt  zspec  Galaxy                          cz  EBVMW  SNe'
printf,li,hdr
;
; open mv script (to rename output files)
mfil=odir+'/mv_script_'+strtrim(samnam,2)+'.csh'
openw,lm,mfil,/get_lun
;
; loop over sample
for i=0L,nsam-1L do begin

    mags = [galsam[i].FUV_int_mag,$
	galsam[i].NUV_int_mag,$
	galsam[i].uJ_int_mag,$
	galsam[i].bJ_int_mag,$
	galsam[i].vJ_int_mag,$
	galsam[i].u_int_mag,$
	galsam[i].g_int_mag,$
	galsam[i].r_int_mag,$
	galsam[i].i_int_mag,$
	galsam[i].z_int_mag,$
	galsam[i].J_int_mag,$
	galsam[i].H_int_mag,$
	galsam[i].K_int_mag,$
	galsam[i].w1_int_mag,$
	galsam[i].w2_int_mag,$
	galsam[i].w3_int_mag,$
	galsam[i].w4_int_mag,$
	galsam[i].i12m_int_mag,$
	galsam[i].i25m_int_mag,$
	galsam[i].i60m_int_mag,$
	galsam[i].i100m_int_mag ]
    g = where(mags ge 0., ng)
    if ng ge ntst then begin

	odat = fltarr(42)
	p = 0L
	odat[p] = (finite(galsam[i].FUV_int_mag) ? galsam[i].FUV_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].FUV_int_magerr) ? galsam[i].FUV_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].NUV_int_mag) ? galsam[i].NUV_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].NUV_int_magerr) ? galsam[i].NUV_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].uJ_int_mag) ? galsam[i].uJ_int_mag+0.809 : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].uJ_int_magerr) ? galsam[i].uJ_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].bJ_int_mag) ? galsam[i].bJ_int_mag-0.098 : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].bJ_int_magerr) ? galsam[i].bJ_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].vJ_int_mag) ? galsam[i].vJ_int_mag+0.012 : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].vJ_int_magerr) ? galsam[i].vJ_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].u_int_mag) ? galsam[i].u_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].u_int_magerr) ? galsam[i].u_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].g_int_mag) ? galsam[i].g_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].g_int_magerr) ? galsam[i].g_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].r_int_mag) ? galsam[i].r_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].r_int_magerr) ? galsam[i].r_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i_int_mag) ? galsam[i].i_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i_int_magerr) ? galsam[i].i_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].z_int_mag) ? galsam[i].z_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].z_int_magerr) ? galsam[i].z_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].J_int_mag) ? galsam[i].J_int_mag+0.896 : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].J_int_magerr) ? galsam[i].J_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].H_int_mag) ? galsam[i].H_int_mag+1.374 : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].H_int_magerr) ? galsam[i].H_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].K_int_mag) ? galsam[i].K_int_mag+1.841 : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].K_int_magerr) ? galsam[i].K_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].w1_int_mag) ? galsam[i].w1_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].w1_int_magerr) ? galsam[i].w1_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].w2_int_mag) ? galsam[i].w2_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].w2_int_magerr) ? galsam[i].w2_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].w3_int_mag) ? galsam[i].w3_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].w3_int_magerr) ? galsam[i].w3_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].w4_int_mag) ? galsam[i].w4_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].w4_int_magerr) ? galsam[i].w4_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i12m_int_mag) ? galsam[i].i12m_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i12m_int_magerr) ? galsam[i].i12m_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i25m_int_mag) ? galsam[i].i25m_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i25m_int_magerr) ? galsam[i].i25m_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i60m_int_mag) ? galsam[i].i60m_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i60m_int_magerr) ? galsam[i].i60m_int_magerr : -9.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i100m_int_mag) ? galsam[i].i100m_int_mag : -99.)
	p = p + 1L
	odat[p] = (finite(galsam[i].i100m_int_magerr) ? galsam[i].i100m_int_magerr : -9.)


    	ispec = 'Id'+string(i+1L,form='(i09)')+'.spec'
    	ospec = strtrim(galsam[i].id,2)+'.spec'
    	printf,lm,'mv '+ispec+' '+ospec

	if strlen(galsam[i].sne) le 0 then $
		sne = '-' $
	else	sne = galsam[i].sne
	printf,li,i+1L, odat, cntxt, (galsam(i).cz/!PHYS_C)>0.001, $
		galsam(i).id,galsam(i).cz, galsam(i).mwebmv, sne, $
		format=fmt
    endif
endfor
;
free_lun,li,lm
;
return
end
