function unxjds,usec
;+
;	unxjds - convert a vector of unix seconds to julian date
;-
	n = n_elements(usec)
	jd = dblarr(n)
	for i=0L,n-1 do jd[i] = unixjd(usec[i])
	return,jd
end
