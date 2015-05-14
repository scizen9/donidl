pro velcomp
;
flist=file_search('*sne.dat',count=nf)
;
th=3
si=1.3
;
cd,'./',current=cwd
sta=strsplit(cwd,'/',count=np,/extract)
mdl=sta(np-1)
;
if nf eq 2 then begin
	!p.multi=[0,1,2]
	readcol,flist(0),sn1,zc1,zh1,dz1,mb1,dmb1
	readcol,flist(1),sn2,zc2,zh2,dz2,mb2,dmb2
	diff=zc1-zc2
	w=where(zc1 lt 0.15)
	mn=mean(diff(w))
	sg=stddev(diff(w))
	plot,zc1,diff(w),psym=4,thick=th,symsi=si,charsi=si,charthi=th, $
		xtitle='z indiv V!Dpec!N', xthick=th, xran=[0.,0.15], xsty=1, $
  		ytitle='(z indiv V!Dpec!N - z DelH!D1!N)', ythick=th, $
		yran=[-0.01,0.01], ysty=1,title=mdl
	oplot,[-5,100],[0,0],linesty=2
	legend,['Mean: '+string(mn,form='(f7.3)'), $
		'Stdev: '+string(sg,form='(f7.3)')],box=0,/right
	diff=mb1-mb2
	mn=mean(diff(w))
	sg=stddev(diff(w))
	plot,mb1,diff(w),psym=4,thick=th,symsi=si,charsi=si,charthi=th, $
		xtitle='mu(B) indiv V!Dpec!N', xthick=th, xran=[14.5,20.0], xsty=1, $
  		ytitle='(mu(B) indiv V!Dpec!N - mu(B) DelH!D1!N)', ythick=th, $
		yran=[-0.03,0.0], ysty=1
	oplot,[-5,100],[0,0],linesty=2
	legend,['Mean: '+string(mn,form='(f7.3)'), $
		'Stdev: '+string(sg,form='(f7.3)')],box=0,/right
	!p.multi=0
endif else print,'Should be 2 *sne.dat files: ',nf
;
return
end
