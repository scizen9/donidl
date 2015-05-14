pro sedfit_site_table
;
; print out the following table:
;
;	sedfit_site.txt	- integrated measures
common sndb_info
;
; get sample
t=where(sndat.smpl_ustr eq 1, nsam)
;t=where(sndat.smpl_str gt 0. and sndat.smpl_str lt 2., nsam)
;t=where(sndat.K_int_mag gt 0. and $
;	sndat.fuv_int_mag gt 0. and sndat.nuv_int_mag gt 0. and $
;	sndat.cz gt -900. and sndat.cz le 20000., nsam)
;t=where((sndat.fuv_exptime gt 100. or sndat.nuv_exptime gt 100.) and $
;	sndat.K_int_mag gt 0. and sndat.cz gt -900., nsam)
;g=get_snsam('C',czlim=20000.,exlim=100.,inclim=70.,nsam=nsam)
snsam=sndat(t)
;
; limit errors
x=where(snsam.FUV_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).FUV_1kpc_magerr = snsam(x).FUV_1kpc_magerr>0.01
x=where(snsam.NUV_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).NUV_1kpc_magerr = snsam(x).NUV_1kpc_magerr>0.01
x=where(snsam.u_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).u_1kpc_magerr = snsam(x).u_1kpc_magerr>0.01
x=where(snsam.g_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).g_1kpc_magerr = snsam(x).g_1kpc_magerr>0.01
x=where(snsam.r_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).r_1kpc_magerr = snsam(x).r_1kpc_magerr>0.01
x=where(snsam.i_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).i_1kpc_magerr = snsam(x).i_1kpc_magerr>0.01
x=where(snsam.z_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).z_1kpc_magerr = snsam(x).z_1kpc_magerr>0.01
x=where(snsam.J_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).J_1kpc_magerr = snsam(x).J_1kpc_magerr>0.01
x=where(snsam.H_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).H_1kpc_magerr = snsam(x).H_1kpc_magerr>0.01
x=where(snsam.K_1kpc_magerr ge 0., nx)
if nx gt 0 then snsam(x).K_1kpc_magerr = snsam(x).K_1kpc_magerr>0.01
;
; open output file
ofil='sedfit_site.txt'
filestamp,ofil
openw,li,ofil,/get_lun
printf,li,'# SN Site (1kpc) Photometry: '+systime(0)
fmt='(a8,f9.1,20f9.2,f7.3,2x,a)'
;
; header
hdr='# SN           cz     FUV      FUVe     NUV      NUVe   u_sdss     usde   g_sdss     gsde   r_sdss     rsde   i_sdss     isde   z_sdss     zsde      J        Je       H        He       K        Ke   EBVMW  Host'
printf,li,hdr
;
; loop over sample
for i=0,nsam-1 do begin
	printf,li,snsam(i).id,snsam(i).cz, $
		snsam(i).FUV_1kpc_mag,snsam(i).FUV_1kpc_magerr, $
		snsam(i).NUV_1kpc_mag,snsam(i).NUV_1kpc_magerr, $
		snsam(i).u_1kpc_mag,snsam(i).u_1kpc_magerr, $
		snsam(i).g_1kpc_mag,snsam(i).g_1kpc_magerr, $
		snsam(i).r_1kpc_mag,snsam(i).r_1kpc_magerr, $
		snsam(i).i_1kpc_mag,snsam(i).i_1kpc_magerr, $
		snsam(i).z_1kpc_mag,snsam(i).z_1kpc_magerr, $
		snsam(i).J_1kpc_mag,snsam(i).J_1kpc_magerr, $
		snsam(i).H_1kpc_mag,snsam(i).H_1kpc_magerr, $
		snsam(i).K_1kpc_mag,snsam(i).K_1kpc_magerr, $
		snsam(i).mwebmv, snsam(i).host, $
		format=fmt
endfor
;
free_lun,li
;
return
end
