pro ploteqgal,obfil

path='/Users/neill/galex/ais_chains/data/'

; read in sky grid positions
sgdat=mrdfits(path+'sky_grid-tdb.fits',1,/silent)

;### remember that grid number is target id - 1

if !d.name eq 'PS' then begin
	!p.font=0
	device,/helv,/isolatin1
	th=2
	ss=0.5
	si=0.5
endif else begin
	th=3
	ss=0.7
	si=1.0
endelse

plot,[0,0],[0,0],xrange=[180,-180],yrange=[-90,90],xsty=5,ysty=5,/nodata
aitoff,sgdat.ra,sgdat.dec,ax,ay
aitoff_grid,/label,thick=th
plots,ax,ay,psym=6,symsize=ss,thick=th

gl = (findgen(1000)/1000.) * 360.
gb = fltarr(1000)
glactc,gra,gdec,2000.,gl,gb,2,/degree
aitoff,gra,gdec,gx,gy
oplot,gx,gy,psym=4,symsize=0.1,thick=th

gl = (findgen(1000)/1000.) * 360.
gb = fltarr(1000) + 20.
glactc,gra,gdec,2000.,gl,gb,2,/degree
aitoff,gra,gdec,gx,gy
oplot,gx,gy,psym=4,symsize=0.1

gl = (findgen(1000)/1000.) * 360.
gb = fltarr(1000) - 20.
glactc,gra,gdec,2000.,gl,gb,2,/degree
aitoff,gra,gdec,gx,gy
oplot,gx,gy,psym=4,symsize=0.1

glactc,gra,gdec,2000.,[0.,0.],[0.,0.],2,/degree
aitoff,gra,gdec,gx,gy
oplot,gx,gy,psym=2,symsize=4.,thick=th

glactc,gra,gdec,2000.,[180.,180.],[0.,0.],2,/degree
aitoff,gra,gdec,gx,gy
oplot,gx,gy,psym=4,symsize=4.,thick=th

if n_params(0) ge 1 then begin
	readcol,obfil,obj,ora,odec,form='a,d,d',/silent
	nob = n_elements(ora)
	for i=0,nob-1 do begin
		aitoff,ora[i],odec[i],ox,oy
		plots,ox,oy,psym=7,symsize=2.,thick=th
		xyouts,ox,oy+1.5,obj[i],align=0.5,charsi=si*1.5
	endfor
endif


for i=0L,n_elements(sgdat)-1L do begin
	xyouts,ax[i],ay[i]+1.5,strn(i),align=0.5,charsi=si
endfor

return
end
