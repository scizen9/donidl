pro snstats,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit, $
	merrlim=merrlim,help=help
;
; GALEX statistics on what's in the DB
;
; get sn data
COMMON lowz_sne_info
;
; check keywords
if keyword_set(help) then begin
	print,'Usage - snstats,czlimit=czlimit,exlimit=exlimit,inclimit=inclimit,merrlim=merrlim'
	return
endif
if keyword_set(czlimit) then $
	czlim = czlimit $
else	czlim = 1.e9
if keyword_set(exlimit) then $
	exlim = exlimit $
else	exlim = 0.
if keyword_set(inclimit) then $
	inclim = inclimit $
else	inclim = 99.9
if keyword_set(merrlim) then $
	melim = merrlim $
else	melim = 1.
;
; types to analyze
tyran = [ $
	[2,4], $        ; Ib, Ib/c, Ic
	[5,9], $        ; II
	[8,8], $	; IIP
	[1,1]]		; Ia
s=size(tyran)
nm=s(2)
apres=sndat(0).ap_res
q=''
;
; How many hosts need integrated UV magnitudes?
;
fl='snst_hostUV.list'
filestamp,fl
openw,ol,fl,/get_lun
printf,ol,'#SNSTATS: '+systime(0)
printf,ol,'#Hosts with deep UV imaging, but no integrated magnitudes'
printf,ol,'#exlim,czlim,inclim: ',exlim,czlim,inclim,format='(a,f9.1,f12.1,f5.1)'
printf,ol,'#'
printf,ol,'# SN        Tyn  Type      Host                  CZ     F_exptm  N_exptm  D(asec)    Kres'
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['tyn','hinc','cz']
        itr = [[tyran(0,i),tyran(1,i)],[-999.,inclim], $
	       [-999.0,czlim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		for j=0,ng-1 do begin
			p=g(j)
			if (sndat(p).fuv_exptime ge exlim and $
			    sndat(p).hfuv_int_mag lt 0.) or $
			   (sndat(p).nuv_exptime ge exlim and $
			    sndat(p).hnuv_int_mag lt 0.) then $
				printf,ol,sndat(p).id,sndat(p).tyn, $
					sndat(p).type,$
					sndat(p).host,sndat(p).cz, $
					sndat(p).fuv_exptime, $
					sndat(p).nuv_exptime, $
					sndat(p).hd25,sndat(p).hK_res_mag, $
				format='(a-10,i5,2x,a-8,2x,a-15,f12.1,3f9.1,f8.2)'
		endfor
	endif
endfor
free_lun,ol
;
; How many SN Ia hosts with LC pars need integrated UV magnitudes?
;
fl='snst_IahostUV.list'
filestamp,fl
openw,ol,fl,/get_lun
printf,ol,'#SNSTATS: '+systime(0)
printf,ol,'#Hosts with deep UV imaging, Ia LC pars, but no integrated magnitudes'
printf,ol,'#exlim,czlim,inclim: ',exlim,czlim,inclim,format='(a,f9.1,f12.1,f5.1)'
printf,ol,'#'
printf,ol,'# SN        Type      Host                  CZ     F_exptm  N_exptm  D(asec) Stretch   Delta'
;
; Ia type
i = 3
;
; get good data
its = ['tyn','hinc','cz']
itr = [[tyran(0,i),tyran(1,i)],[-999.,inclim],[-999.0,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	for j=0,ng-1 do begin
		p=g(j)
		if ((sndat(p).fuv_exptime ge exlim and $
		     sndat(p).hfuv_int_mag lt 0.) or $
		    (sndat(p).nuv_exptime ge exlim and $
		     sndat(p).hnuv_int_mag lt 0.)) and $
		   (sndat(p).smpl_str gt 0. or  $
		    sndat(p).mlcs2k_delta gt -8.) then $
			printf,ol,sndat(p).id,sndat(p).type,sndat(p).host, $
				sndat(p).cz, sndat(p).fuv_exptime, $
				sndat(p).nuv_exptime, sndat(p).hd25, $
				sndat(p).smpl_str,sndat(p).mlcs2k_delta, $
		format='(a-10,2x,a-8,2x,a-15,f12.1,3f9.1,2f8.2)'
	endfor
endif
free_lun,ol
;
; How many SN Ia hosts with LC pars need SN UV magnitudes?
;
fl='snst_Ia_noUV.list'
filestamp,fl
openw,ol,fl,/get_lun
printf,ol,'#SNSTATS: '+systime(0)
printf,ol,'#Hosts with Ia LC pars, but no deep UV imaging'
printf,ol,'#czlim,inclim: ',czlim,inclim,format='(a,f12.1,f5.1)'
printf,ol,'#'
printf,ol,'# SN        Type      Host                  CZ     D(asec) Stretch   Delta'
;
; Ia type
i = 3
;
; get good data
its = ['tyn','hinc','cz']
itr = [[tyran(0,i),tyran(1,i)],[-999.0,inclim],[-999.0,czlim]]
g = snsample(its,itr,count=ng)
if ng gt 0 then begin
	for j=0,ng-1 do begin
		p=g(j)
		if (sndat(p).hfuv_int_mag le 0 or $
		    sndat(p).hnuv_int_mag le 0) and $
		   (sndat(p).smpl_str gt 0. or  $
		    sndat(p).mlcs2k_delta gt -8.) then $
			printf,ol,sndat(p).id,sndat(p).type,sndat(p).host, $
				sndat(p).cz, sndat(p).hd25, $
				sndat(p).smpl_str,sndat(p).mlcs2k_delta, $
				format='(a-10,2x,a-8,2x,a-15,f12.1,f9.1,2f8.2)'
	endfor
endif
free_lun,ol
;
; How many hosts need site IR magnitudes?
;
fl='snst_siteIR.list'
filestamp,fl
openw,ol,fl,/get_lun
printf,ol,'\ SNSTATS: '+systime(0)
printf,ol,'\ hosts with no IR site magnitudes'
printf,ol,'\ czlim: ',czlim,format='(a,f12.1)'
printf,ol,'\ inclim: ',inclim,format='(a,f5.1)'
printf,ol,'\EQUINOX = ''J2000.0'''
printf,ol,'| ra            | dec           | radius    | name          |'
printf,ol,'| double        | double        | double    | char          |'
;
; loop over types
for i=0,nm-1 do begin
;
; get good data
	its = ['tyn','hJ_res_mag','hH_res_mag','hK_res_mag','hinc','cz']
        itr = [[tyran(0,i),tyran(1,i)],[-999.,0.],[-999.,0.],[-999.,0.], $
	       [-999.,inclim],[-999.0,czlim]]
        g = snsample(its,itr,count=ng)
	if ng gt 0 then begin
		for j=0,ng-1 do begin
			p=g(j)
			printf,ol,sndat(p).hra,sndat(p).hdec,$
				sndat(p).hd25,sndat(p).host, $
				format='(f14.8,2x,f14.8,f9.1,7x,a13)'
		endfor
	endif
endfor
free_lun,ol
;
; How many hosts need host IR magnitudes?
;
fl='snst_hostIR.list'
filestamp,fl
openw,ol,fl,/get_lun
printf,ol,'\ SNSTATS: '+systime(0)
printf,ol,'\ hosts with no integrated IR magnitudes'
printf,ol,'\ czlim: ',czlim,format='(a,f12.1)'
printf,ol,'\ inclim: ',inclim,format='(a,f5.1)'
printf,ol,'\EQUINOX = ''J2000.0'''
printf,ol,'| ra            | dec           | major     | name          |'
printf,ol,'| double        | double        | double    | char          |'
;
; loop over types
for i=0,n_elements(sndat)-1 do begin
;
	if sndat(i).hK_int_mag lt 0. and sndat(i).cz le czlim then begin
		printf,ol,sndat(i).hra,sndat(i).hdec,$
			sndat(i).hd25>10.<300.,sndat(i).host, $
			format='(f14.8,2x,f14.8,f9.1,7x,a18)'
	endif
endfor
free_lun,ol
;
return
end
