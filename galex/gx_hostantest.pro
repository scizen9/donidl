pro gx_hostantest
;
flist=file_search(!SNE_SITES+'I*/*/snhostani.log',count=nf)
;
nxpt=[0.]
ndel=[0.]
fxpt=[0.]
fdel=[0.]
for i=0,nf-1 do begin
	readcol,flist(i),host,fm,fme,nm,nme,sn,surv,fexpt,nexpt, $
		format='a,f,f,f,f,a,a,f,f',/silent
	t=where(strpos(surv,'AIS') lt 0,n)
	if n gt 0 then begin
		mxi=where(fexpt eq max(fexpt))
		fmx=fm(mxi(0))
		if fmx gt 0 then begin
		  dl =where(fexpt ne max(fexpt) and strpos(surv,'AIS') ge 0,ndl)
		  for j=0,ndl-1 do begin
			if fm(dl(j)) gt 0. and fm(dl(j)) lt 30. then begin
				del = fmx-fm(dl(j))
				if del gt 1.5 then print,flist(i)
				fdel = [fdel,del]
				fxpt = [fxpt,fexpt(dl(j))]
			endif
		  endfor
		endif
		mxi=where(nexpt eq max(nexpt))
		nmx=nm(mxi(0))
		if nmx gt 0 then begin
		  dl =where(nexpt ne max(nexpt) and strpos(surv,'AIS') ge 0,ndl)
		  for j=0,ndl-1 do begin
			if nm(dl(j)) gt 0. and nm(dl(j)) lt 30. then begin
				del = nmx-nm(dl(j))
				if del gt 1.5 then print,flist(i)
				ndel = [ndel,del]
				nxpt = [nxpt,nexpt(dl(j))]
			endif
		  endfor
		endif
	endif
endfor
;
nxpt=nxpt(1:*)
ndel=ndel(1:*)
fxpt=fxpt(1:*)
fdel=fdel(1:*)
;
; statistics
nres=moment(ndel,sdev=nsg)
fres=moment(fdel,sdev=fsg)
;
!p.multi=[0,1,2]
th=3
si=1.5
fmt='(f5.2)'
;
plot,nxpt,ndel,psym=5,xtitle='EXPTIME',ytitle='Delta NUV Mag',$
	charsi=si,charthi=th,thick=th,yran=[-1.,1.]
oplot,[0,1000],[0.,0.]
legend,['NUV: '+string(nres(0),form=fmt)+' +- '+string(nsg,form=fmt),$
	''],box=0,/right,charsi=1.7,charthi=3
plot,fxpt,fdel,psym=6,xtitle='EXPTIME',ytitle='Delta FUV Mag',$
	charsi=si,charthi=th,thick=th,yran=[-1.,1.]
oplot,[0,1000],[0.,0.]
legend,['FUV: '+string(fres(0),form=fmt)+' +- '+string(fsg,form=fmt),$
	''],box=0,/right,charsi=1.7,charthi=3
;
return
end
