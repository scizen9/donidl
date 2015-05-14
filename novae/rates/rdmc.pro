pro rdmc,ifl,rates,matches,ntrials,field,set,type
;
openr,1,ifl
;
rec = ''
;
; get field id
readf,1,rec
for i=0,7 do field = gettok(rec,' ')
;
; get ntrials
readf,1,rec
junk = gettok(rec,' ')
junk = gettok(rec,' ')
ntrials = long(gettok(rec,' '))
if n_params(0) gt 5 then begin
	for i=0,4 do junk = gettok(rec,' ')
	set = gettok(rec,',')
endif
if n_params(0) gt 6 then $
	type = rec
;
maxrd = 200
rates = intarr(maxrd)
matches = lonarr(maxrd)
irate = 0
imat = 0L
;
nrd = 0
while not eof(1) and nrd lt maxrd do begin
	readf,1,irate,imat
	rates(nrd) = irate
	matches(nrd) = imat
	nrd = nrd + 1
endwhile
nrd = nrd - 1
;
rates = rates(0:nrd)
matches = matches(0:nrd)
;
close,1
;
return
end
