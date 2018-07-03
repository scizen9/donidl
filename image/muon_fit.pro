pro muon_fit, gplot=gplot, mplot=mplot, hplot=hplot, ps=ps
;+
;  muon_fit - fit muons in the dark image
;-
; initial params
bs=0.025
cd,current=cwd
sta=strsplit(cwd,'/',/extract)
cwd = sta[n_elements(sta)-1]
; read data
;
; get image list
imlist = file_search('im??.fits', count=nf)
allbig = fltarr(5000)
allsmall = fltarr(5000)
p = 0L
;
; loop over images
for k=0,nf-1 do begin
    imf = imlist[k]
    im = mrdfits(imf, 0, imhdr)
    seg = mrdfits(strmid(imf, 0, strpos(imf,'.'))+'_seg.fits', 0, sghdr)
    readcol,strmid(imf, 0, strpos(imf,'.'))+'_cr.cat', $
	sids, xs, ys, xrms, yrms, thetas, format='i,f,f,f,f,f', $
	comment='#'
    ;
    ; convert thetas to radians
    thetas = thetas * !pi / 180.
    ;
    nseg = n_elements(sids)
    big = fltarr(nseg) - 1.
    small = fltarr(nseg) - 1.
    q=''
    ;
    for j = 0, nseg-1 do begin
	;
	; get ID
	sno = sids[j]
	;
	; get costheta
	costh = cos(thetas[j])
	;
	; get x points
	t = where(seg eq sno,nt)
	if nt gt 0 then begin
		ind = array_indices(seg, t)
		xs = minmax(ind[0,*])
		nx = (xs[1] - xs[0]) + 1
		;
		; get larger muons within a certain angle
		if yrms[j] le 0.6 and yrms[j] ge 0.52 and costh > 0.7 and $
	   	nx gt 9 then begin
			xx = indgen(nx) + xs[0]
			sg = fltarr(nx)
			;
			; loop over x points
			for i = 0, nx-1 do begin
				;
				; fit CR
				sig = muon_sample(im, seg, sno, xx[i], gplot=gplot)
				;
				; store results
				sg[i] = sig
			endfor
			g = where(xx gt 0 and sg gt 0., ng)
			if ng gt 0 then begin
				xx = xx[g]
				sg = sg[g]
				sgth = sg*costh
				coef = linfit(xx,sgth,/double,yfit=yfit)
				diff = sgth-yfit
				ims_asym,diff,md,sgd,wgt,siglim=[3.,2.]
				g = where(wgt eq 1)
				b = where(wgt ne 1,nb)
				;print,'nrej: '+strn(nb)
				;xx = xx[g]
				;sg = sg[g]
				coef = linfit(xx[g],sgth[g],/double)
				yfit = coef[0] + xx * coef[1]
				if keyword_set(mplot) then begin
					plot,xx,sg,psym=-5, title='Seg: '+strn(sno)
					oplot,xx,sgth,psym=-6
					oplot,xx,yfit,linesty=2,thick=5
					if nb gt 0 then oplot,xx[b],sgth[b], $
								psym=2,symsi=2.
					read,'next: ',q
				endif
				;
				; store values
				mm = minmax(yfit)
				small[j] = mm[0]
				big[j] = mm[1]
				allsmall[p] = mm[0]
				allbig[p] = mm[1]
				p += 1
				;read,'next: ',q
			endif else print,'No good points'
		endif
    	endif else print,'Could not find segment '+strn(sno)
    endfor
    ;
    ; clean
    g = where(big gt 0, ng)
    big = big[g]
    small = small[g]
    print,imf+':  N CRs = '+strn(ng)
    ;
    ; final histo
    if keyword_set(hplot) then begin
    	plothist,big,bin=bs,xran=[0.,1.5], title=imf
    	plothist,small,bin=bs,/overplot,linesty=2
    	read,'next: ',q
    endif
;
; end loop over files
endfor
g = where(allbig gt 0, ng)
allbig = allbig[g]
allsmall = allsmall[g]
print,'N Crs: '+strn(ng)
diflen = sqrt(allbig*allbig - allsmall*allsmall) * 15.0
hd = histogram(diflen,bin=0.1,max=50.,loc=hx)

if keyword_set(ps) then begin
	psfile,cwd+'_difflen'
	!p.font=0
endif
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=5
si=1.75

tlab = cwd + ' - ALL '+strn(nf)+' images, '+strn(ng)+' muon CRs'
plot,hx,hd,psym=10,title=tlab,xran=[5,15],/xs, $
	xthick=th,xtitle='Diffusion Length (micrometers)',xcharsi=si, $
	ythick=th,ytitle='N',ycharsi=si, $
	charthi=th,charsi=si
medl = median(diflen)
oplot,[medl,medl],!y.crange
print,'Median diffusion length (microns): ',medl,format='(a,f9.3)'
;
if keyword_set(ps) then psclose
;
return
end
