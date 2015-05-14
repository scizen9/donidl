pro gx_snhostimrg
;
; Merge statistics on SNe
;
; read log file
readcols,'snhostani.log',host,mgf,mgfe,mgn,mgne,sn,srv,fexpt,nexpt,snx,sny, $
	apr,fbckg,nbckg,fn,/silent,format='a,f,f,f,f,a,a,f,f,f,f,f,f,f,a'
nr=n_elements(host)
;
; open mrg file
lfile='snhostimrg.log'
filestamp,lfile
openw,ol,lfile,/get_lun
printf,ol,'# SNHOSTIMRG run on '+systime(0)
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
			fext=fexpt(w)
			next=nexpt(w)
			fbck=fbckg(w)
			nbck=nbckg(w)
			s=sort(imn)
			fmg=fmg(s)
			fmge=fmge(s)
			nmg=nmg(s)
			nmge=nmge(s)
			imn=imn(s)
			fext=fext(s)
			next=next(s)
			fbck=fbck(s)
			nbck=nbck(s)
			u=uniq(imn)
			fmg=fmg(u)
			fmge=fmge(u)
			nmg=nmg(u)
			nmge=nmge(u)
			next=next(u)
			fext=fext(u)
			fbck=fbck(u)
			nbck=nbck(u)
;
; NUV
;
; now use expt to exclude AIS ( < 500s )
			ngd=where(next gt 270., nngd)
			if nngd gt 0 then $
			    gx_avmags,1,nmg(ngd),nmge(ngd),next(ngd), $
			    	wmgn,  wmgne,  nxpt, nan, nbackg, $
			    	background=nbck(ngd), /error_weights, /silent $
			else begin
			    wmgn=-99.9
			    wmgne=-9.99
			    nxpt=0.
			    nan=0
			    nbackg=0.
			endelse
;
; FUV
;
; now use expt to exclude AIS ( < 500s )
			fgd=where(fext gt 270., nfgd)
			if nfgd gt 0 then $
			    gx_avmags,2,fmg(fgd),fmge(fgd),fext(fgd), $
			    	wmgf,  wmgfe,  fxpt, naf, fbackg, $
			    	background=fbck(fgd), /error_weights, /silent $
			else begin
			    wmgf=-99.9
			    wmgfe=-9.99
			    fxpt=0.
			    naf=0
			    fbackg=0.
			endelse
;
; set mag error floor
			if wmgf ge 0. then wmgfe = wmgfe>0.01
			if wmgn ge 0. then wmgne = wmgne>0.01
;
; write out results
			if wmgn gt 0. or wmgf gt 0. then begin
			  a = w(0)
    			  printf,ol,host(a),wmgf,wmgfe,wmgn,wmgne, $
				apr(a),fxpt,nxpt,naf,nan,fbackg,nbackg, $
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
