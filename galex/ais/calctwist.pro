pro calctwist,ra,dec,twist
	;
    chlen = n_elements(ra)
    qarr=dblarr(4,chlen)
    twist=dblarr(chlen)
    q1=rdt2q([ra[0],dec[0],0.0d])
    qarr(*,0) = q1
    rdt=q2rdt(qarr[*,0])
    print,rdt,format='(3F11.6)'
    twist[0] = rdt[2]
    for j=1,chlen-1 do begin
    	q0=rdt2q([ra[j],dec[j],0.0])
        qdiff=qmult(qnorm(qconj(qarr[*,j-1])),qnorm(q0))
        qdiff[2] = 0.0d
        qdelta=qnorm(qdiff)
        qarr[*,j]=qmult(qnorm(qarr[*,j-1]),qdelta)
        rdt=q2rdt(qarr[*,j])
	print,rdt,format='(3F11.6)'
    	twist[j] = rdt[2]
    endfor

end
