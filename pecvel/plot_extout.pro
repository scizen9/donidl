pro plot_extout,ifile
;
openr,il,ifile,/get_lun
;
rec=''
while not eof(il) do begin
	readf,il,rec
	if strpos(rec,'w:') ge 0 then begin
		jnk=gettok(rec,' ')
		w = float(rec)
	endif
	if strpos(rec,'Om:') ge 0 then begin
		jnk=gettok(rec,' ')
		om = float(rec)
	endif
	if strpos(rec,'Ode:') ge 0 then begin
		jnk=gettok(rec,' ')
		ode = float(rec)
	endif
	if strpos(rec,'Sm:') ge 0 then begin
		jnk=gettok(rec,' ')
		sm = float(rec)
	endif
endwhile
free_lun,il
;
readcol,ifile,sn,zcmb,zhel,rmag,rmerr,dlsm,corr,fmag,fmerr,mdiff,chi2,sig,$
	wid,dwid,col,dcol, format='a,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f'
;
mub=fmag+sm
dif= dlsm - fmag
!p.multi=[0,1,2]
plot,zcmb,mub,psym=4,ysty=1,xsty=1,xtitle='z(CMB)',charsi=1.5,charthi=3,$
	ytitle='mu(B)',title=ifile,xthick=3,ythick=3
legend,['w: '+string(w,form='(f6.3)'),'Om: '+string(om,form='(f6.4)'), $
	'Ode: '+string(ode,form='(f6.4)'),'Sm: '+string(sm,form='(f6.3)')], $
	/right,/bottom,charsi=1.5,charthi=3,box=0
plot,zcmb,dif,psym=4,xsty=1,xtitle='z(CMB)',charsi=1.5,charthi=3,$
	ytitle='delta mu(B)',xthick=3,ythick=3
oplot,[-100,100],[0,0],linesty=2
;
return
end
