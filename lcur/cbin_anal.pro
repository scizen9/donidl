pro cbin_anal
;
readcol,'all.cnd',fld,id,x,y,ra,dec,imag,imerr,umb,umberr,bmv,bmverr,$
		  vmi,vmierr,var,qual,per,type, $
		  format='a,l,f,f,a,a,f,f,f,f,f,f,f,f,f,i,d,a'
;
; cull out what we want
g=where(strpos(type,'EW') ge 0 and qual lt 4 and per lt 5.0, ng)
print,'Found ',strn(ng),' good candidates'
;
th=3
si=1.75
!p.multi=[0,1,2]
;
plot,per(g),bmv(g),psym=4,thick=th,xthick=th,ythick=th,charthi=th,$
	charsi=si,xtitle='Period(d)',ytitle='B-V',title='EW', $
	yran=[0,5]
;
q=''
;read,'next: ',q
;
bsi=0.05
h=histogram(per(g),min=0.1,max=1.0,bins=bsi,loc=xh)
xh=xh+bsi/2.
plot,xh,h,psym=10,thick=th,xthick=th,ythick=th,charthi=th,charsi=si,$
	xtitle='Period(d)',ytitle='N',title='', xsty=1
;
!p.multi=0
return
end
