pro gx_snhostmrg
;
; Merge statistics on SNe
;
; read log file
readcols,'snhostan.log',host,mgf,mgfe,mgn,mgne,sn,srv,expt,snx,sny,apr,bckg,fn,$
	/silent,format='a,f,f,f,f,a,a,f,f,f,f,f,a'
nr=n_elements(host)
;
; open mrg file
lfile='snhostmrg.log'
filestamp,lfile
openw,ol,lfile,/get_lun
printf,ol,'# SNHOSTMRG run on '+systime(0)
printf,ol,'# host             mF   mFerr    mN   mNerr  apr(px)    fexpt    nexpt   nf   nn  Fbackg   Nbackg'
;
; loop over hosts
for i=0,nr-1 do begin
;
; check to see if we've looked at this host already (flag with '')
	if strlen(host(i)) ge 3 then begin
		w=where(strcmp(host,host(i)), nh)
		if nh ge 1 then begin
;
; get uniqe measurements
			fmg=mgf(w)
			fmge=mgfe(w)
			nmg=mgn(w)
			nmge=mgne(w)
			imn=fn(w)
			ext=expt(w)
			bck=bckg(w)
			s=sort(imn)
			fmg=fmg(s)
			fmge=fmge(s)
			nmg=nmg(s)
			nmge=nmge(s)
			imn=imn(s)
			ext=ext(s)
			bck=bck(s)
			u=uniq(imn)
			fmg=fmg(u)
			fmge=fmge(u)
			nmg=nmg(u)
			nmge=nmge(u)
			ext=ext(u)
			bck=bck(u)
			goo=where(ext gt 270., ngoo)
			if ngoo gt 0 then begin
;
; mag with NUV resolution aperture
; NUV
			    gx_avmags,1,nmg(goo),nmge(goo),ext(goo), $
			    	wmgn,  wmgne,  nexpt, nan, nbackg, $
			    	background=bck(goo), /error_weights, /silent
;
; FUV
			    gx_avmags,2,fmg(goo),fmge(goo),ext(goo), $
			    	wmgf,  wmgfe,  fexpt, naf, fbackg, $
			    	background=bck(goo), /error_weights, /silent
;
; set mag error floor
			    if wmgf ge 0. then wmgfe = wmgfe>0.01
			    if wmgn ge 0. then wmgne = wmgne>0.01
;
; write out results
			    a = w(0)
    			    printf,ol,host(a),wmgf,wmgfe,wmgn,wmgne, $
				apr(a),fexpt,nexpt,naf,nan,fbackg,nbackg, $
				format='(a-15,4f7.2,3f9.1,2i5,2x,e9.3,2x,e9.3)'
			endif
    			host(w) = ''
		endif
	endif
endfor
;
free_lun,ol
;
return
end
