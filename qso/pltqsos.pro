pro pltqsos,lfile,expcut=expcut,sncut=sncut,yrange=yrange, $
	fuv_fvar_sort=fuv_fvar_sort, $
	nuv_fvar_sort=nuv_fvar_sort, $
	fuv_nobs_sort=fuv_nobs_sort, $
	nuv_nobs_sort=nuv_nobs_sort, $
	fuv_avmg_sort=fuv_avmg_sort, $
	nuv_avmg_sort=nuv_avmg_sort, $
	z_sort=z_sort, $
	loiii_sort=loiii_sort
;+
; pltqsos - plot photometry of qso's in list file
;-
flist=file_search(lfile,count=nf)
if nf ne 1 then begin
	print,'PLTQSOS: Error - file not found: ',lfile
	return
endif
;
readcol,lfile,fv,nv,fn,nn,famg,namg,ra,dec,z,loiiig,loiiinp, $
	format='f,f,i,i,f,f,a,a,f,f,f'
ntp=n_elements(fv)
;
; check sorting
if keyword_set(z_sort) then begin
	print,'Sorting: redshift'
	s=sort(z)
	xx = z
	ylab='z'
endif else if keyword_set(loiii_sort) then begin
	print,'Sorting: L(OIII)np'
	s=sort(loiiinp)
	xx = loiiinp
	ylab='L(OIII)'
endif else if keyword_set(nuv_avmg_sort) then begin
	print,'Sorting: NUV avg mag'
	s=sort(namg)
	xx = namg
	ylab='NUV AV MAG'
endif else if keyword_set(fuv_avmg_sort) then begin
	print,'Sorting: FUV avg mag'
	s=sort(famg)
	xx = famg
	ylab='FUV AV MAG'
endif else if keyword_set(nuv_nobs_sort) then begin
	print,'Sorting: NUV Nobs'
	s=sort(nn)
	xx = nn
	ylab='NUV Nobs'
endif else if keyword_set(fuv_nobs_sort) then begin
	print,'Sorting: FUV Nobs'
	s=sort(fn)
	xx = fn
	ylab='FUV Nobs'
endif else if keyword_set(nuv_fvar_sort) then begin
	print,'Sorting: NUV Fvar'
	s=reverse(sort(nv))
	xx = nv
	ylab='NUV Fvar'
endif else if keyword_set(fuv_fvar_sort) then begin
	print,'Sorting: FUV Fvar'
	s=reverse(sort(fv))
	xx = fv
	ylab='FUV Fvar'
endif
;
; sort vectors
ra=ra(s)
z=z(s)
famg=famg(s)
namg=namg(s)
loiiinp=loiiinp(s)
nn=nn(s)
fn=fn(s)
nv=nv(s)
fv=fv(s)
xx=xx(s)
;
; plot trends
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
!p.multi=[0,1,2]
th=3
si=1.5
xran=[-0.01,0.75]
g=where(fv ge 0. and xx gt 0.)
if strpos(ylab,'MAG') ge 0 then begin
	yran=[max(xx(g)),min(xx(g))]
	del=yran[0]-yran[1]
	yran[0]=yran[0]+del/20.
	yran[1]=yran[1]-del/20.
endif else begin
	yran=[min(xx(g)),max(xx(g))]
	del=yran[1]-yran[0]
	yran[0]=yran[0]-del/20.
	yran[1]=yran[1]+del/20.
endelse
plot,fv(g),xx(g),thick=th,xthick=th,ythick=th,charsi=si,charthi=th,/nodata, $
	xtitle='FUV Fvar', xran=xran, xsty=1, title=lfile, $
	ytitle=ylab, yran=yran, ysty=1
oplot,fv(g),xx(g),psym=5,symsi=si,thick=th,color=colordex('P')
g=where(nv ge 0. and xx gt 0.)
plot,nv(g),xx(g),thick=th,xthick=th,ythick=th,charsi=si,charthi=th,/nodata, $
	xtitle='NUV Fvar', xran=xran, xsty=1, $
	ytitle=ylab, yran=yran, ysty=1
oplot,nv(g),xx(g),psym=6,symsi=si,thick=th,color=colordex('B')
!p.multi=0
;
q=''
i=0L
inc=1L
read,'next: ',q
;
while i ge 0 and i lt ntp do begin
	flist=file_search(strtrim(ra(i),2)+'*.pht',count=nf)
	if nf ne 1 then begin
		print,'No file for QSO at ra: ',ra(i)
	endif else begin
		if keyword_set(yrange) then $
		   pltqsophot,flist(0),expcut=expcut,sncut=sncut,yrange=yrange $
		else	pltqsophot,flist(0),expcut=expcut,sncut=sncut
		hdr = 'Fav, Nav, z, L(OIII): ' + $
			string(famg(i),form='(f7.3)') + ', ' + $
			string(namg(i),form='(f7.3)') + ', ' + $
			string(z(i),form='(f6.4)') + ', ' + $
			string(loiiinp(i),form='(f5.2)')
		read,hdr+' - '+strtrim(i,2)+'/'+strtrim(ntp,2)+ $
			', inc ['+strn(inc)+']: ',q
		if strtrim(q,2) eq '' then $
			i = i + inc $
		else	if strupcase(strtrim(q,2)) eq 'Q' then $
			i = i + ntp + 1000L $
		else	begin
			inc = long(strtrim(q,2))
			i = i + inc
		endelse
	endelse
endwhile
;
return
end
