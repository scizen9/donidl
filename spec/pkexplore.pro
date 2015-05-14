pro pkexplore,x,y,wid,sth,ath,pkg
;
pks = findpeaks(x,y,wid,sth,ath,pkg,/verbose)
;
d = smooth(deriv(y),wid)
;
nwin = fix( (max(x) - min(x)) / 100 )
print,'nwin: ',nwin

q=''
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
!p.multi=[0,1,2]
;
for i=0,nwin do begin
	x0 = min(x) + float(i) * 100.
	x1 = x0 + 100.
	plot,x,y,xran=[x0,x1]
	oplot,!x.crange,[ath,ath]
	t=where(x gt x0 and x lt x1)
	y0 = max(y[t]) - max(y[t])*0.05
	oplot,pks,replicate(y0,n_elements(pks)),psym=4
	plot,x,d,xran=[x0,x1]
	oplot,pks,replicate(0,n_elements(pks)),psym=2
	oplot,!x.crange,[0,0]
	read,'next: ',q
endfor
return
end
