function get_snlist,ifil,nsam=nsam,hosts=hosts,silent=silent
;+
; get_snlist - get indices for sndat from list in a file (ifil)
; ifil - list of sne, one per line
; nsam - set to return variable for number of sne
; hosts - set to return unique list of hosts (no duplicates)
;-
;
COMMON sndb_info
;
readcol,ifil,isn,format='a',silent=silent
n=n_elements(isn)
;
sam=lonarr(n)-1L
;
hlist = strarr(n)
for i=0,n-1 do begin
	s=snfind(isn[i],/silent)
	if s ge 0 then begin
		sam[i]=s[0]
		hlist[i]=sndat[s[0]].host
	endif
endfor
t=where(sam ge 0)
sam = sam[t]
if keyword_set(hosts) then begin
	hlist = hlist[t]
	s = sort(hlist)
	hlist = hlist[s]
	sam   = sam[s]
	u = uniq(hlist)
	sam   = sam[u]
	s = sort(sam)
	sam   = sam[s]
endif
;
nsam = n_elements(sam)
;
return,sam
end
