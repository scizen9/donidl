pro plotchains,ais,aisflg,astr,fieldsize,npixel,galex_radius,chain,label, $
	i100limit,sgdat,aislim,gexp,ocgrid

; part of mkchain
; updated for replanning AIS to 30k limit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

graph_color=getcolor('white',13)

theta=findgen(361)/!RADEG

euler,astr.crval(0),astr.crval(1),gl,gb,1



plot,[0,1],[0,1],/nodata,xrange=[0.5*fieldsize+1,-0.5*fieldsize-1],xstyle=1,$
yrange=[-0.5*fieldsize-1,0.5*fieldsize+1],ystyle=1,xtitle='!7D!XRA',$
ytitle='!7D!XDec',ymargin=[5,10],xmargin=[10,6],xthick=thk,ythick=thk,$
charsize=1.2,color=graph_color,$
subtitle='Region center:' +adstring(astr.crval(0),astr.crval(1),0)+'  ('+ $
string(astr.crval(0),format='(f7.3)')+' '+ $
string(astr.crval(1),format='(f7.3)')+') ('+string(gl,format='(f7.3)')+' '+ $
string(gb,format='(f7.3)')+')'

oplot,7.0*cos(theta),7.0*sin(theta)

gcirc,2,sgdat.ra,sgdat.dec,astr.crval(0),astr.crval(1),darc
p=where(darc/3600. le fieldsize,np)
if np gt 0 then begin
  ad2xy,sgdat[p].ra,sgdat[p].dec,astr,xg,yg
  xg=(xg-npixel/2)*astr.cdelt(0)
  yg=(yg-npixel/2)*astr.cdelt(1)
  skg=strcompress(sgdat[p].TARGET_ID,/remove_all)
  skg=strmid(skg,6,3)
  skgi=fix(skg) -1
  skg=string(skgi,format='(i3)')
  p=where(sqrt((xg*xg)+(yg*yg)) gt 1.0,np)
  xg=xg(p) & yg=yg(p) & skg=skg(p)
  for i=0,np-1 do begin
    if xg(i) gt 0.0 then xyouts,xg(i)+3.0,yg(i),skg(i),charsize=1.5,color=getcolor('red',10),noclip=0
    if xg(i) lt 0.0 then xyouts,xg(i)-3.0,yg(i),skg(i),charsize=1.5,alignment=1,color=getcolor('red',10),noclip=0
    oplot,7.0*cos(theta)+xg(i),7.0*sin(theta)+yg(i),color=getcolor('red',10),linestyle=2,noclip=0
  endfor
endif

;;project field centers into image coordinates
ad2xy,ais.ra,ais.dec,astr,xgal,ygal
xgal=(xgal-npixel/2)*astr.cdelt(0)
ygal=(ygal-npixel/2)*astr.cdelt(1)
n=50
theta=2*!pi/(1.*n)*findgen(n+1)
xcirc=galex_radius*cos(theta)
ycirc=galex_radius*sin(theta)

; NEWCODE

; plot AIS grid circles
for i=0,n_elements(xgal)-1 do oplot,xcirc+xgal(i),ycirc+ygal(i)

;overplot those centers eliminated due to field brightness limit (NUV = 80k FUV = 30k)
w=where(aislim.fld eq 1,nw)
if nw gt 0  then for i=0,nw-1 do polyfill,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('tan',10),noclip=0
if nw gt 0  then for i=0,nw-1 do oplot,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('chocolate',10),noclip=0

;overplot those centers eliminated due to star brightness limit (NUV = 30k FUV = 7k)
w=where(aislim.bs eq 1,nw)
if nw gt 0  then for i=0,nw-1 do polyfill,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('indian red',10),noclip=0
if nw gt 0  then for i=0,nw-1 do oplot,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('Firebrick',10),noclip=0

; hash mark bad fiels & bad BS
w=where(aislim.bs eq 1 and aislim.fld eq 1,nw)
if nw gt 0  then for i=0,nw-1 do polyfill,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('tan',10),/line_fill,orientation=45,thick=2,noclip=0

; overplot fields with FUV exposure > 60s
w=where(gexp.GFEXP gt 60.0,nw)
; overplot fields with NUV exposure > 60s
w=where(gexp.GNEXP gt 60.0,nw)
if nw gt 0 then for i=0,nw-1 do polyfill,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('sky blue',10),noclip=0

; overplot completed fields with FUV exposure >= 100s
;w=where(gexp.GFEXP ge 100.0,nw)
; overplot completed fields with NUV exposure >= 100s
w=where(gexp.GNEXP ge 100.0,nw)
if nw gt 0 then for i=0,nw-1 do polyfill,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('steel blue',10),noclip=0
if nw gt 0  then for i=0,nw-1 do oplot,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('navy',10),noclip=0

;---
; plot overcountrate shutdown data
;;project field centers into image coordinates
if ocgrid[0].ECLIPSE ne 0 then begin
ad2xy,ocgrid.ra,ocgrid.dec,astr,xoc,yoc
xoc=(xoc-npixel/2)*astr.cdelt(0)
yoc=(yoc-npixel/2)*astr.cdelt(1)
for i=0,n_elements(ocgrid.ra)-1 do polyfill,xcirc+xoc(i),ycirc+yoc(i),color=fsc_color('red',13),noclip=0
print, 'Overcountrate shutdown fields (in RED):'
for i=0,n_elements(ocgrid.ra)-1 do print, ocgrid[i].target_id
endif



;---


;overplot good fields
w=where(aislim.bs eq 0 and aislim.fld eq 0 and gexp.GFEXP lt 100.,nw)
if nw gt 0 then for i=0,n_elements(w)-1 do oplot,xcirc+xgal(w(i)),ycirc+ygal(w(i)),noclip=0


;;next plot + those fields which are new read in from chains.dat
w=where(aisflg eq 1,nw)
if nw gt 0 then begin
    for i=0,nw-1 do begin
	plots,xgal(w(i)),ygal(w(i)),psym=2,symsize=40.0/fieldsize,color=fsc_color('gold',10),noclip=0
	plots,xgal(w(i)),ygal(w(i)),psym=4,symsize=20.0/fieldsize,color=fsc_color('gold',10),noclip=0
    endfor
endif

;;next plot + those fields which are new read in from chains-*.dat
w=where(aisflg eq 5,nw)
if nw gt 0 then begin
    for i=0,nw-1 do begin
	plots,xgal(w(i)),ygal(w(i)),psym=2,symsize=40.0/fieldsize,color=fsc_color('purple',10),noclip=0
	plots,xgal(w(i)),ygal(w(i)),psym=4,symsize=20.0/fieldsize,color=fsc_color('purple',10),noclip=0
    endfor
endif

;; overplot bright stars
ybs=mrdfits('/Users/neill/galex/ais_chains/data/YBS.fits',1,/silent)

gcirc,2,ybs.ra,ybs.dec,astr.crval(0),astr.crval(1),darc
p=where(darc/3600. le (fieldsize/sqrt(2.)) and ybs.mag lt 5.0,np)
if np gt 0 then begin
    ybs = ybs[p]
    ad2xy,ybs.ra,ybs.dec,astr,xyb,yyb
    xyb=(xyb-npixel/2)*astr.cdelt(0)
    yyb=(yyb-npixel/2)*astr.cdelt(1)
    p = where(abs(xyb) le 11. and abs(yyb) le 11., np)
    if np gt 0 then begin
	ybs = ybs[p] & xyb = xyb[p] & yyb = yyb[p]
	s=reverse(sort(ybs.mag))
	xyb = xyb[s] & yyb = yyb[s] & ybs = ybs[s]
	print,'Bright Stars:'
	print,'    FldX    FldY  Name              Type  Mag'
	for i=0,np-1 do begin
			plots,xyb[i],yyb[i],psym=2,symsize=40.0/fieldsize,color=fsc_color('cyan',10),noclip=0
			plots,xyb[i],yyb[i],psym=4,symsize=20.0/fieldsize,color=fsc_color('cyan',10),noclip=0
			print,xyb[i],yyb[i],ybs[i].name,ybs[i].type,ybs[i].mag,$
				form='(2f8.2,2x,a-16,2x,a,f5.2)'
	endfor
    endif
endif
;
; overplot objects
if file_test('objects.ad') eq 1 then begin
	readcol,'objects.ad',obj,ora,odec,form='a,d,d',/silent
	gcirc,2,ora,odec,astr.crval[0],astr.crval[1],darc
	p=where(darc/3600. le fieldsize/sqrt(2.), np)
	if np gt 0 then begin
		obj = obj[p] & ora = ora[p] & odec = odec[p]
		ad2xy,ora,odec,astr,xob,yob
		xob=(xob-npixel/2)*astr.cdelt(0)
		yob=(yob-npixel/2)*astr.cdelt(1)
		p = where(abs(xob) le 11. and abs(yob) le 11., np)
		if np gt 0 then begin
			obj = obj[p] & xob = xob[p] & yob = yob[p]
			print,'Objects:'
			print,'    FldX    FldY  Name'
			for i=0,np-1 do begin
				plots,xob[i],yob[i],psym=4,symsize=60.0/fieldsize,color=fsc_color('cyan',10),noclip=0
				plots,xob[i],yob[i],psym=4,symsize=30.0/fieldsize,color=fsc_color('cyan',10),noclip=0
				print,xob[i],yob[i],obj[i],form='(2f8.2,2x,a-16)'
			endfor
		endif
	endif
endif


; plot overcountrate shutdown data
;;project field centers into image coordinates
if ocgrid[0].ECLIPSE ne 0 then begin
ad2xy,ocgrid.ra,ocgrid.dec,astr,xoc,yoc
xoc=(xoc-npixel/2)*astr.cdelt(0)
yoc=(yoc-npixel/2)*astr.cdelt(1)
for i=0,n_elements(ocgrid.ra)-1 do plots,xoc(i),yoc(i),color=fsc_color('red',13),psym=2,symsize=40.0/fieldsize
for i=0,n_elements(ocgrid.ra)-1 do plots,xoc(i),yoc(i),color=fsc_color('red',13),psym=4,symsize=20.0/fieldsize
;print, 'Overcountrate shutdown fields (in RED):'
;for i=0,n_elements(ocgrid.ra)-1 do print, ocgrid[i].target_id
endif

;;--------
;; plot specific positions (for TESTING)
;ad2xy,87.629619,56.336302,astr,txgal,tygal
;txgal=(txgal-npixel/2)*astr.cdelt(0)
;tygal=(tygal-npixel/2)*astr.cdelt(1)
;plots,txgal,tygal,psym=7,symsize=3,color=getcolor('green',10)
;ad2xy,94.906739,49.302572,astr,txgal,tygal
;txgal=(txgal-npixel/2)*astr.cdelt(0)
;tygal=(tygal-npixel/2)*astr.cdelt(1)
;plots,txgal,tygal,psym=1,symsize=3,color=getcolor('green',10)
;;--------



;--------

; plot fields that have exposure but are bad for some reason
w=where(aislim.fld eq 1 and gexp.GFEXP ge 60.,nw)
if nw gt 0  then for i=0,nw-1 do polyfill,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('tan',10),/line_fill,orientation=135,thick=2,noclip=0

w=where(aislim.bs eq 1 and gexp.GFEXP ge 60.,nw)
if nw gt 0  then for i=0,nw-1 do polyfill,xcirc+xgal(w(i)),ycirc+ygal(w(i)),color=fsc_color('indian red',10),/line_fill,orientation=135,thick=2,noclip=0


oplot,7.0*cos(theta),7.0*sin(theta)

i=0
while chain(i).id(0) gt 0 do begin
	plots,chain(i).x(0),chain(i).y(0),psym=4,color=getcolor('red',13),$
	symsize=1.5,thick=4
	xyouts,chain(i).x(0)-0.5,chain(i).y(0)-0.5,strcompress(string(i+1,$
	format='(i2)'),/remove_all),color=getcolor('red',13),charsize=3,$
	charthick=2
	n=chain(i).ntile
	plots,chain(i).x(n-1),chain(i).y(n-1),psym=4,$
	color=getcolor('blue',13),thick=4,symsize=1.5
	n=chain(i).ntile
	oplot,chain(i).x(0:n-1),chain(i).y(0:n-1) ,color=getcolor('green',13),thick=2
	i=i+1
endwhile

xyouts,-0.25*fieldsize,0.6*fieldsize,'ACCEPT',/data
xyouts, 0.35*fieldsize,0.6*fieldsize,'REJECT',/data
xyouts, 0.025*fieldsize,0.6*fieldsize,'QUIT',/data
xyouts, 0.14*fieldsize,0.77*fieldsize,label,charsize=1.5,/data,color=getcolor('red',10)


plots,[1,1,-1,-1,1]*.09*fieldsize,([1,-1,-1,1,1]*.04+0.6)*fieldsize
plots,([1,1,-1,-1,1]*.19-0.3)*fieldsize,([1,-1,-1,1,1]*.04+0.6)*fieldsize
plots,([1,1,-1,-1,1]*.19+0.3)*fieldsize,([1,-1,-1,1,1]*.04+0.6)*fieldsize


; key
plots,0.65*fieldsize,0.71*fieldsize,psym=2,/data,symsize=2,color=fsc_color('purple',10)
plots,0.65*fieldsize,0.71*fieldsize,psym=4,/data,color=fsc_color('purple',10)
xyouts,0.62*fieldsize,0.7*fieldsize,'NEW Plans',/data,charsize=1.2


polyfill,(xcirc*0.7)+(0.375*fieldsize),(ycirc*0.7)+(0.705*fieldsize),color=fsc_color('tan',10),/fill
;oplot,(xcirc*0.7)+(0.375*fieldsize),(ycirc*0.7)+(0.705*fieldsize),color=fsc_color('chocolate',10),noclip=0,thick=2
xyouts,0.33*fieldsize,0.71*fieldsize,'Fail Field',/data,charsize=1.2
xyouts,0.33*fieldsize,0.68*fieldsize,'brightness',/data,charsize=1.2


polyfill,(xcirc*0.7)-(0.15*fieldsize),(ycirc*0.7)+(0.705*fieldsize),color=fsc_color('indian red',10),/fill
oplot,(xcirc*0.7)-(0.15*fieldsize),(ycirc*0.7)+(0.705*fieldsize),color=fsc_color('Firebrick',10),thick=2
xyouts,-0.2*fieldsize,0.71*fieldsize,'Failed',/data,charsize=1.2
xyouts,-0.2*fieldsize,0.68*fieldsize,'Bright Star',/data,charsize=1.2

polyfill,(xcirc*0.7)+(0.11*fieldsize),(ycirc*0.7)+(0.735*fieldsize),color=fsc_color('sky blue',10),/fill
oplot,(xcirc*0.7)+(0.11*fieldsize),(ycirc*0.7)+(0.735*fieldsize)
;xyouts,0.08*fieldsize,0.73*fieldsize,'FUV > 60s',/data,charsize=1.2
xyouts,0.08*fieldsize,0.73*fieldsize,'NUV > 60s',/data,charsize=1.2

polyfill,(xcirc*0.7)+(0.11*fieldsize),(ycirc*0.7)+(0.675*fieldsize),color=fsc_color('steel blue',10),/fill
oplot,(xcirc*0.7)+(0.11*fieldsize),(ycirc*0.7)+(0.675*fieldsize),color=fsc_color('navy',10),thick=2
;xyouts,0.08*fieldsize,0.67*fieldsize,'FUV >= 100s',/data,charsize=1.2
xyouts,0.08*fieldsize,0.67*fieldsize,'NUV >= 100s',/data,charsize=1.2

; for don

;ad2xy,ocgrid.ra,ocgrid.dec,astr,xoc,yoc
;ad2xy,300.00875,30.3666111,astr,d1x,d1y
;ad2xy,299.50625,30.334167,astr,d2x,d2y
;plots,d1x,d1y,psym=6,symsize=3,color=fsc_color('green',12)
;plots,d2x,d2y,psym=1,symsize=3,color=fsc_color('green',12)


return
end

