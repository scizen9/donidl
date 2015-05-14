function gx_cts_to_jy,cts,band,fuv=fuv,nuv=nuv
	if keyword_set(fuv) then $
		jy = cts*1.40e-15*(1531.6^2/2.99e18)*1.0e23 $
	else	jy = cts*2.06e-16*(2256.6^2/2.99e18)*1.0e23
	return,jy
end
