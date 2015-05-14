pro gx_spec_plot,fname,w,f,fe,smoo=smoo,llist=llist
;
; read in and plot spectrum
;
; spectral features
sfea = [{wv: 912.,  label: 'LL',          psLabel: 'LL' }, $
	{wv: 937.,  label: 'S VI',        psLabel: 'S VI'  }, $
	{wv: 973.,  label: 'Ly !4c!3',    psLabel: 'Ly !9g!3' }, $
	{wv: 991.,  label: 'N III',       psLabel: 'N III' }, $
	{wv: 1026., label: 'Ly !4b!3',    psLabel: 'Ly !9b!3' }, $
	{wv: 1034., label: 'O VI',        psLabel: 'O VI' }, $
	{wv: 1216., label: 'Ly !4a!3',    psLabel: 'Ly !9a!3' }, $
	{wv: 1260., label: 'Si II',       psLabel: 'Si II' }, $
	{wv: 1303., label: 'O I + Si II', psLabel: 'O I + Si II' }, $
	{wv: 1334., label: 'C II',        psLabel: 'C II' }, $
	{wv: 1397., label: 'Si IV',       psLabel: 'Si IV' }, $
	{wv: 1467.5, label: 'Ni II',      psLabel: 'Ni II' }, $
	{wv: 1486., label: 'S I',         psLabel: 'S I' }, $
	{wv: 1493., label: 'N I',         psLabel: 'N I' }, $
	{wv: 1526.7, label: 'Si II',      psLabel: 'Si II' }, $
	{wv: 1533.4, label: 'Si II',      psLabel: 'Si II' }, $
	{wv: 1549., label: 'C IV',        psLabel: 'C IV' }, $
	{wv: 1640.5, label: 'He II',      psLabel: 'He II' }, $
	{wv: 1670.8, label: 'Al II',      psLabel: 'Al II' }, $
	{wv: 1909., label: 'C III]',      psLabel: 'C III]' }, $
	{wv: 2800., label: 'Mg II',       psLabel: 'Mg II' } ]
;
readcol,fname,w,f,fe,format='f,f,f',/silent
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.8
; title
cd,'.',cur=cwd
tmp = strmid(cwd,strpos(cwd,'/g/')+1)
dir = gettok(tmp,'-')
tmp = fname
obid = gettok(tmp,'.')
tile = strmid(obid,0,strpos(obid,'_',/reverse_search))
obid = strmid(obid,strpos(obid,'_',/reverse_search)+1)
tlab = dir + ' ' + tile + ' ' + obid
; smooth
if keyword_set(smoo) then $
	sf = smooth(f,smoo) $
else 	sf = f
;
g = where(w gt 1350. and w lt 2950.)
yrng = [-(max(sf(g))*0.1), max(sf(g))]
;
plot,w,sf,thick=th,xthick=th,ythick=th,charsi=si, charthi=th, title=tlab, $
	xrange=[1320.,3000.],xsty=1,xtitle='Wavelength (Ang)', $
	yrange=yrng,ysty=1,ytitle='Flux Density (ergs cm!U-2!Ns!U-1!NAng!U-1!N)'
oplot,w,fe,color=colordex('R')
;
; spectral features
if keyword_set(llist) then begin
    inc = max(sf(g))*0.02
    n = n_elements(llist)
    if n ge 1 then $
	for i=0,n-1 do begin
	    t=where(sfea.wv eq llist(i),nt)
	    if nt eq 1 then begin
		s=where(w gt llist(i))
		y0 = max(sf(s(0)-10:s(0)+10)) + inc
		oplot,[llist(i),llist(i)],[y0,y0+inc],thick=th
		xyouts,llist(i)-10.,y0+inc*2,sfea(t(0)).label, $
			charsi=si,charthi=th
	    endif else print,'Not found: ',llist(i)
	endfor $
    else print,'No features found'
endif
;
return
end
