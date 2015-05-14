pro snhsed_int_table,t,samnam,saminfo,lem,testmags=testmags
;
; print out the following table:
;
;	snhsed_int.txt	- integrated measures
common sndb_info
common galdb_info
;
if n_params(0) lt 4 then begin
	print,'Usage: snhsed_int_table, <indx_list>, <samnam>, <saminfo>, <limits_legend>, /testmags'
	return
endif
;
; get sample
snsam=sndat(t)
nsam=n_elements(t)
hind=snsam.hind
g = where(hind ge 0,nsam)
snsam = snsma[g]
hind = hind[g]
galsam = galsam[hind]
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
ofil='snhsed_int_sam'+strtrim(samnam,2)+'.txt'
filestamp,ofil
openw,li,ofil,/get_lun
printf,li,'# SNHSED_INT_TABLE: '+systime(0)
if keyword_set(testmags) then $
	printf,li,'# Testing Mags: at least 3 must be >= 0' $
else	printf,li,'# NOT Testing Mags'
printf,li,'# '+saminfo
for i=0,n_elements(lem)-1 do $
	printf,li,'# '+lem(i)
printf,li,'#'

fmt='(a10,f9.1,34f9.2,f7.3,2x,a)'
;
; header
hdr='# SN             cz     FUV      FUVe     NUV      NUVe      U        Ue       B        Be       V        Ve    u_sdss     usde   g_sdss     gsde   r_sdss     rsde   i_sdss     isde   z_sdss     zsde      J        Je       H        He       K        Ke      12mu    12mue     25mu    25mue     60mu    60mue    100mu   100mue  EBVMW  Host'
printf,li,hdr
;
; loop over sample
for i=0,nsam-1 do begin
    mags = [galsam(i).FUV_int_mag,$
	galsam(i).NUV_int_mag,$
	galsam(i).uJ_int_mag,$
	galsam(i).bJ_int_mag,$
	galsam(i).vJ_int_mag,$
	galsam(i).u_int_mag,$
	galsam(i).g_int_mag,$
	galsam(i).r_int_mag,$
	galsam(i).i_int_mag,$
	galsam(i).z_int_mag,$
	galsam(i).J_int_mag,$
	galsam(i).H_int_mag,$
	galsam(i).K_int_mag,$
	galsam(i).i12m_int_mag,$
	galsam(i).i25m_int_mag,$
	galsam(i).i60m_int_mag,$
	galsam(i).i100m_int_mag ]
    g = where(mags ge 0., ng)
    if ng ge 3 or not keyword_set(testmags) then $
	printf,li,snsam(i).id,galsam(i).cz, $
		galsam(i).FUV_int_mag,galsam(i).FUV_int_magerr, $
		galsam(i).NUV_int_mag,galsam(i).NUV_int_magerr, $
		galsam(i).uJ_int_mag,galsam(i).uJ_int_magerr, $
		galsam(i).bJ_int_mag,galsam(i).bJ_int_magerr, $
		galsam(i).vJ_int_mag,galsam(i).vJ_int_magerr, $
		galsam(i).u_int_mag,galsam(i).u_int_magerr, $
		galsam(i).g_int_mag,galsam(i).g_int_magerr, $
		galsam(i).r_int_mag,galsam(i).r_int_magerr, $
		galsam(i).i_int_mag,galsam(i).i_int_magerr, $
		galsam(i).z_int_mag,galsam(i).z_int_magerr, $
		galsam(i).J_int_mag,galsam(i).J_int_magerr, $
		galsam(i).H_int_mag,galsam(i).H_int_magerr, $
		galsam(i).K_int_mag,galsam(i).K_int_magerr, $
		galsam(i).i12m_int_mag,galsam(i).i12m_int_magerr, $
		galsam(i).i25m_int_mag,galsam(i).i25m_int_magerr, $
		galsam(i).i60m_int_mag,galsam(i).i60m_int_magerr, $
		galsam(i).i100m_int_mag,galsam(i).i100m_int_magerr, $
		galsam(i).mwebmv, galsam(i).hlname, $
		format=fmt
endfor
;
free_lun,li
;
return
end
