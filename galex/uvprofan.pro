pro uvprofan,ps=ps
;
; perform profile analysis on uv profile for SN locations
;
; sn data
common lowz_sne_info
;
; get merged file
mfil=file_search('snmrg.log',count=nf)
if nf lt 1 then begin
	print,'snmrg.log not found.'
	return
endif
;
; setups
th=3
si=2.5
csi=1.75
zpf=18.82
zpn=20.02
fsave=!p.font
!p.multi=[0,1,2]
;
; open log file
lfile='uvprofan.log'
filestamp,lfile
openw,ol,lfile,/get_lun
printf,ol,'# UVPROFAN run on '+systime(0)
printf,ol,'#SN       delmagN delmagNe  delmagF delmagFe   delcol  delcole  lfracN  lfracF      cz      riso   requiv host'
;
; open err file
efile='uvprofan.err'
filestamp,efile
openw,el,efile,/get_lun
printf,el,'# UVPROFAN: Missing profile list, run on '+systime(0)
printf,el,'# GAL                  cz  SN        SNtype'
;
; open input file
openr,il,mfil(0),/get_lun
rec=''
while not eof(il) do begin
    readf,il,rec
    if strpos(rec,'#') lt 0 then begin
	sta=strsplit(rec,/extract)
;
; SN basic data
	sn=sta(0)
	p=snfind(sn)
	sntyp=sndat(p).type
;
; get photometry
	resf=float(sta(1))
	resfe=float(sta(2))
	sbf=float(sta(9))
	resn=float(sta(10))
	resne=float(sta(11))
	sbn=float(sta(18))
;
; get sn relative position
	snx=sndat(p).off_ew
	sny=sndat(p).off_ns
	ginc=sndat(p).hinc
	gpa=sndat(p).hpa
	if snx gt -9000. and sny gt -9000. then begin
		if ginc gt 0. and gpa gt 0. then begin
			riso = isorad(gpa,snx,sny,inc=ginc,b=b)
			reqv = sqrt(riso*b)	; equivalent radius
		endif else begin
			reqv = sqrt(snx^2+sny^2)
			riso = reqv
		endelse
	endif else begin
		reqv = -99.
		a = -99.
	endelse
;
; get velocity (cz)
	cz = sndat(p).cz
;
; print data
	print,sn,snx,sny,ginc,gpa,reqv,sbn,sbf,cz,form='(a-8,7f9.2,f11.1)'
;
; get NGA profile
	host=sta(19)
	gal=get_nga_name(host,/silent)
	gx_profile,gal,rn,mn,mne,rf,mf,mfe,rfmn,mfmn,mfmne,q,pa,ra,dec,/silent
;
; profile found?
	if q ge 0 then begin
;
; analyze profile
;
; get offsets from radial profile and cumulative light internal to sn
;
; NUV
		nfrac = -9.
		delmn = -99.
		delmne= -99.
		labn  = ''
		labnf = ''
;
; valid profile?
		if min(rn) ge 0. then begin
;
; get cumulative light
			cnt = 10.0^(-0.4*(mn-zpn))
			cumn=cnt-cnt
			for i=1,n_elements(mn)-1 do $
				cumn(i) = cumn(i-1) + cnt(i-1)*(!pi*rn(i)^2 - $
							        !pi*rn(i-1)^2)
			cumn = cumn / max(cumn)
;
; where is sn on galaxy?
			t=where(rn gt reqv,n)
;
; inside galaxy profile?
			if n gt 0 then begin
				nfrac = interpol(cumn,rn,reqv)
				rmagn=interpol(mn,rn,reqv)
;
; get delta mag
			    if sbn gt -9. then begin
				delmn=sbn-rmagn
				delmne=interpol(mne,rn,reqv)
				delmne=sqrt(delmne^2+resne^2)
				labn = '!9D!3NUV = '+string(delmn,form='(f6.2)')
			    endif
;
; outside of galaxy?
			endif else nfrac = 1.
			labnf= 'NUV LFRAC = '+string(nfrac,form='(f6.3)')
;
; no valid NUV profile
		endif else printf,el,'# No NUV profile for ',gal
;
; FUV
		ffrac = -9.
		delmf = -99.
		delmfe= -99.
		labf  = ''
		labff = ''
;
; valid profile?
		if min(rf) ge 0. then begin
;
; get cumulative light
			cnt = 10.0^(-0.4*(mf-zpf))
			cumf=cnt-cnt
			for i=1,n_elements(mf)-1 do $
				cumf(i) = cumf(i-1) + cnt(i-1)*(!pi*rf(i)^2 - $
							        !pi*rf(i-1)^2)
			cumf = cumf / max(cumf)
;
; where is sn on galaxy?
			t=where(rf gt reqv,n)
;
; inside galaxy profile?
			if n gt 0 then begin
				ffrac = interpol(cumf,rf,reqv)
				rmagf=interpol(mf,rf,reqv)
;
; get delta mag
			    if sbf gt -9. then begin
				delmf=sbf-rmagf
				delmfe=interpol(mfe,rf,reqv)
				delmfe=sqrt(delmfe^2+resfe^2)
				labf = '!9D!3FUV = '+string(delmf,form='(f6.2)')
			    endif
;
; outside of galaxy?
			endif else ffrac = 1.
			labff= 'FUV LFRAC = '+string(ffrac,form='(f6.3)')
;
; no valid FUV profile
		endif else printf,el,'# No FUV profile for ',gal
;
; FUV - NUV
		if delmn gt -90. and delmf gt -90. and rfmn(0) ge 0. then begin
			rmagc=interpol(mfmn,rfmn,reqv)
			delc=rmagc-(sbf-sbn)
			delce=interpol(mfmne,rfmn,reqv)
			labc = '!9D!3(FUV-NUV) = '+string(delc,form='(f6.2)')
		endif else begin
			delc = -99.
			delce= -99.
			labc = ''
		endelse
;
; record results in log file
		printf,ol,sn,delmn,delmne,delmf,delmfe,delc,delce, $
			nfrac,ffrac,cz,riso,reqv,gal, $
			format='(a-8,6f9.2,2f8.3,3f9.1,1x,a)'
;
; plot results
		if keyword_set(ps) then begin
			pfl=sn+'/'+gal+'_'+sn+'_UVprof'
			psfile,pfl
			!p.font=1
			ylab=textoidl('\mu (mag arcsec^{-2})')
			yflab=textoidl('Fraction of light < Radius')
			xlab=textoidl('Radius (arcsec)')
		endif else begin
			ylab='!9n!3 (mag arcsec!U-2!N)'
			yflab='Fraction of light < Radius'
			xlab='Radius (arcsec)'
		endelse
;
; plot profile
		tar = [mn,mf,sbf,sbn]
		good=where(tar gt 0)
		mxmag = 32.
		mnmag = 19.
		maxrad= max([rn,rf,rfmn,reqv])+5.
		bx=[0.,0.]
		by=[0.,0.]
		plot,bx,by,psym=4,thick=th,charsi=csi,xthick=th,ythick=th,$
			yran=[mxmag,mnmag],ytitle=ylab,ysty=1, $
			xran=[-2,maxrad],xtitle=xlab,xsty=1, $
			charthi=th,title=sn+', '+host+' '+systime(0), $
			/nodata
		oplot,[reqv,reqv],[-100,100]
		if rn(0) ge 0. then begin
			oplot,rn,mn,psym=4,thick=th
			errplot,rn,mn-mne,mn+mne,thick=th
			plots,reqv,sbn,psym=4,thick=th,symsi=si*2.
			errplot,[reqv,reqv],[sbn,sbn]-resne,[sbn,sbn]+resne,$
				thick=th
		endif
		if rf(0) ge 0. then begin
			oplot,rf,mf,psym=5,thick=th
			errplot,rf,mf-mfe,mf+mfe,thick=th
			plots,reqv,sbf,psym=5,thick=th,symsi=si*2.
			errplot,[reqv,reqv],[sbf,sbf]-resfe,[sbf,sbf]+resfe,$
				thick=th
		endif
		legend,[labn,labf,labc],box=0,charthi=th,charsi=csi,/bottom
		legend,['NUV','FUV'],psym=[4,5],charthi=th,charsi=csi,$
			thick=[th,th],/right
;
; plot cumulate light
		plot,bx,by,psym=4,thick=th,charsi=csi,xthick=th,ythick=th,$
			yran=[-0.05,1.1],ytitle=yflab,ysty=1, $
			xran=[-2,maxrad],xtitle=xlab,xsty=1, $
			charthi=th,/nodata
		oplot,[reqv,reqv],[-100,100]
		if rn(0) ge 0. then $
			oplot,rn,cumn,psym=4,thick=th
		if rf(0) ge 0. then $
			oplot,rf,cumf,psym=5,thick=th
		legend,[labnf,labff],charsi=csi,charthi=th,box=0,/right,/bottom
;
; next plot?
		if keyword_set(ps) then $
			psclose $
		else	read,'next: ',rec
		if strupcase(strmid(rec,0,1)) eq 'Q' then begin
			free_lun,il,ol,el
			!p.font=fsave
			!p.multi=0
			return
		endif
;
; no profile found
	endif else printf,el,gal,cz,sn,sntyp,format='(a-15,f10.1,2x,a-10,a-8)'
    endif
endwhile
;
free_lun,il,ol,el
!p.font=fsave
!p.multi=0
;
return
end
