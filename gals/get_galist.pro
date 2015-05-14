function get_galist,ifil,nsam=nsam,silent=silent
;+
; get_galist - get indices for galdat from list in a file (ifil)
; ifil - list of galaxies, one per line
; nsam - set to return variable for number of galaxies
;-
;
COMMON galdb_info
;
readcol,ifil,igal,format='a',silent=silent,comment='#'
nsam=n_elements(igal)
sam=lonarr(nsam)
;
for i=0L,nsam-1L do begin
	g=gfind(igal[i],silent=silent)
	sam[i]=g[0]
endfor
t=where(sam ge 0)
sam = sam[t]
;
nsam = n_elements(sam)
;
return,sam
end
