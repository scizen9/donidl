pro cv_ecl_plt
;
q=findgen(100)/100
t=where(q ge 0.1)
q = q(t)
plot,q,100.*cv_ecl(q),thick=3,charthick=3,charsize=2, $
	xtitle='q = M(2)/M(1)',ytitle='Eclipse Probability (%)', $
	yran=[21,37],ysty=1,xran=[0.0,1.1],xsty=1,xthick=2,ythick=2
;
return
end
