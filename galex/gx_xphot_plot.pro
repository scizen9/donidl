pro gx_xphot_plot,delt,title=title,ps=ps,verbose=verbose
;+
;	gx_xphot_plot - plot a light curve from a set of *-mchx.fits files
;
; INPUTS:
;	delt	- time sampling in seconds
;
; KEYWORDS:
;	title	- title for plot (also used for ps output filename)
;	ps	- set to generate postscript plot
;
; HISTORY:
;	02-aug-2011, jdn - Initial Version
;-
;
fzp=18.82	; FUV zero point
nzp=20.08	; NUV zero point
;
; assume we have at least NUV files
flist=file_search('*-nd-mchx.fits',count=nf)
if nf le 0 then begin
	print,'GX_XPHOT_PLOT: Error - No *-nd-mchx.fits files found'
	return
endif
if keyword_set(title) then begin
	tlab = title
	psf = strcompress(tlab,/remove)
endif else begin
	tlab = ''
	psf = 'gx_xphot_plot'
endelse
;
; plot setup
font_save = !p.font
if keyword_set(ps) then begin
	psfile,psf
	!p.font = 0
	th=3
	si=1.5
endif else begin
	th=3
	si=1.75
endelse
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
;
for i=0,nf-1 do begin
	;
	; NUV
	gx_xphot,flist[i],delt,ntim,nctz,epoch=ep0,verbose=verbose
	t0 = min(ntim)
	s2n = (nctz*delt) / sqrt(nctz*delt)
	nmag = -2.5*alog10(nctz) + nzp
	nmer = 1.0857362 / s2n
	;
	; FUV
	fp = repstr(flist[i],'-nd-','-fd-')
	if file_test(fp) then begin
		gx_xphot,fp,delt,ftim,fctz,verbose=verbose
		s2n = (fctz*delt) / sqrt(fctz*delt)
		fmag = -2.5*alog10(fctz) + fzp
		fmer = 1.0857362 / s2n
	endif else begin
		fmag = [-1]
		ftim = [-1]
		fmer = [-1]
	endelse
	;
	xrng = minmax(ntim-t0)
	del = max(xrng) - min(xrng)
	xrng[0] = xrng[0] - del*0.1
	xrng[1] = xrng[1] + del*0.1
	;
	ysam = [nmag+nmer,nmag-nmer,fmag+fmer,fmag-fmer]
	g = where(ysam gt 0.)
	yrng = [ max([ysam[g]]), min([ysam[g]]) ]
	del = max(yrng) - min(yrng)
	yrng[0] = yrng[0] + del*0.1
	yrng[1] = yrng[1] - del*0.3
	;
	plot,ntim-t0,nmag,charsi=si,charthi=th,xthick=th,ythick=th,title=tlab, $
		xtitle='Time: '+ep0+' + (s)',xran=xrng,xsty=1, $
		ytitle='AB MAG',yran=yrng,ysty=1,/nodata
	oplot,ntim-t0,nmag,psym=4,thick=th,symsi=si,color=colordex('B')
	oploterror,ntim-t0,nmag,nmer,psym=3
	oplot,ftim-t0,fmag,psym=5,thick=th,symsi=si,color=colordex('P')
	oploterror,ftim-t0,fmag,fmer,psym=3
	;
	legend,['FUV','NUV'],psym=[5,4],symsi=[si,si],thick=[th,th], $
		color=[colordex('P'),colordex('B')],/right,charsi=si, $
		box=0,charthi=th
	legend,[textoidl('\DeltaT = ' + $
			 strtrim(string(delt,form='(f9.1)'),2)+'s'), ' '], $
		/left,charsi=si,box=0,charthi=th
	;
	q=''
	if not keyword_set(ps) then $
		read,'next <cr>: ',q
endfor
if keyword_set(ps) then psclose
!p.font = font_save
return
end
