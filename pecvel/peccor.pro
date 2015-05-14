pro peccor,nname,z,mub,pname,vpec,zpec,mubpec,pvel,nomubcor=nomubcor
;
zpec=z-z
mubpec=mub-mub
pvel=z-z
;
; speed of light
c=299792.458d0
;
nn=n_elements(mub)
;
for i=0,nn-1 do begin
    t=where(strpos(pname,strmid(nname(i),2,4)) ge 0,nf)
    if nf eq 1 then begin
        pvel(i) = vpec(t(0))
        zpec(i) = (1.+z(i))/(1.+pvel(i)/c) - 1.
	if keyword_set(nomubcor) then $
		mubpec(i) = mub(i) $
        else	mubpec(i) = mub(i)-5.0*alog10(1.+2.*pvel(i)/c)
    endif else begin
        zpec(i) = z(i)
        mubpec(i) = mub(i)
        pvel(i) = 0.
    endelse
endfor
;
return
end ; pro peccor
