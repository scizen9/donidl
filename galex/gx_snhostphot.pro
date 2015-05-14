pro gx_snhostphot
;
; Get GALEX and red photometry for the SN hosts
;
; get list of files
flist=file_search('*_list.txt',count=nf)
if nf le 0 then begin
	print,'No *_list.txt files found'
	return
endif
;
; setup
c=2.99792458d5	; speed o' light
;
; get NGA data
readcol,'~/snsites/NGA/uv_mags.dat',hnga,rflst,rnlst,flst,flste,nlst,nlste,$
	fasm,fasme,nasm,nasme,format='a,f,f,f,f,f,f,f,f,f,f',/silent
;
; loop over inputs
for i=0,nf-1 do begin
	tmp=flist(i)
	type=gettok(tmp,'_list')
	openw,ol,type+'_hphot.dat',/get_lun
	readcol,flist(i),snnm,snra,sndec,sntype,host,hra,hdec, $
		ofew,ofns,htp,htc,vz,form='a,d,d,a,a,d,d,f,f,a,f,f',/silent
	nsn=n_elements(snra)
	print,flist(i),nsn,type,format='(a-15,i7,2x,a-8)'
;
; loop over hosts
	for j=0,nsn-1 do begin
;
; get NGA galaxy name
		gal=host(j)
        	p=stregex(gal,'[0-9]')
		cat=strmid(gal,0,p)
		if strcmp(cat,'E') then begin   ; ESO galaxy catalog
			cat='ESO'
			b=stregex(gal,'-')
			n1=fix(strmid(gal,p,b-p))
			n2=fix(strmid(gal,b+2))
			gal=cat+string(n1,form='(i03)')+'-G'+ $
				string(n2,form='(i03)')
		endif else if strcmp(cat,'IC') or $
		      	strcmp(cat,'NGC') then begin
			n1=fix(strmid(gal,p))
			gal=cat+string(n1,form='(i04)')
		endif else if strcmp(cat,'PGC') or $
		      	strcmp(cat,'UGC') then begin
			n1=long(strmid(gal,p))
			gal=cat+string(n1,form='(i05)')
		endif
		a=where(strpos(hnga,gal) ge 0, na)
		if na gt 0 then begin
			a=a(0)
			fmg=fasm(a)
			fmge=fasme(a)
			nmg=nasm(a)
			nmge=nasme(a)
		endif else $
			get_ned_phot,host(j),fuv=fmg,errfuv=fmge, $
					     nuv=nmg,errnuv=nmge,/silent

		get_ned_phot,host(j),r_t=rmg,errr_t=rmge, $
				     k_t=kmg,errk_t=kmge,/silent
		if fmg gt 0. and nmg gt 0. then begin
			get_ned1d,host(j),mu=mu,/silent
			if mu lt 0. then $
				get_ned,host(j),coslummu=mu,/silent
			if vz(j) gt 0. and vz(j) lt 2. then $
				cz = vz(j) * c $
			else	cz = vz(j)
			printf,ol,snnm(j),host(j),hra(j),hdec(j),htp(j),htc(j),$
				cz,mu,fmg,fmge,nmg,nmge,rmg,rmge,kmg,kmge,$
		    format='(a-8,2x,a-14,2f13.8,1x,a-5,f7.1,f9.1,9f7.2)'
		endif
	endfor
	free_lun,ol
endfor
;
return
end
