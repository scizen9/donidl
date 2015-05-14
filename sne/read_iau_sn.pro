pro read_iau_sn,sn,host,date,hra,hdec,off,mag,dref,snra,sndec,pref,type
;
ifile=!SNE_DATA+'/IAU_SN.dat'
if file_test(ifile,/read) then begin
	readcol,ifile,sn,host,date,hra,hdec,of1,of2,mag,dref,$
		snra,sndec,pref,type, $
	format='a,a,a,a,a,a,a,f,a,a,a,a,a'
	n=n_elements(sn)
	off=strarr(n)
	for i=0,n-1 do off(i)=of1(i)+' '+of2(i)
endif else begin
;
; setups
sn=['']
host=['']
date=['']
hra=['']
hdec=['']
off=['']
mag=[0.]
dref=['']
snra=['']
sndec=['']
pref=['']
type=['']
;
tfile=!SNE_DATA+'/IAU_SN.txt'
openr,1,tfile
;
while not eof(1) do begin
	rec=' '
	readf,1,rec
	if strlen(rec) gt 1 and strpos(rec,'Host') lt 0 then begin
		sn=[sn,strtrim(strmid(rec,0,7),2)]
		host=[host,strtrim(strmid(rec,8,17),2)]
		date=[date,strtrim(strmid(rec,25,10),2)]
		hra=[hra,strtrim(strmid(rec,37,7),2)]
		hdec=[hdec,strtrim(strmid(rec,45,6),2)]
		off=[off,strtrim(strmid(rec,52,10),2)]
		mstr=strtrim(strmid(rec,62,6),2)
		if mstr ne '' then $
			mag=[mag,float(strmid(rec,62,6))] $
		else	mag=[mag,0.]
		dref=[dref,strtrim(strmid(rec,71,16),2)]
		snra=[snra,strtrim(strmid(rec,87,11),2)]
		sndec=[sndec,strtrim(strmid(rec,99,11),2)]
		pref=[pref,strtrim(strmid(rec,113,17),2)]
		type=[type,strtrim(strmid(rec,130,6),2)]
	endif
endwhile
;
close,1
;
sn=sn(1:*)
host=host(1:*)
date=date(1:*)
hra=hra(1:*)
hdec=hdec(1:*)
off=off(1:*)
mag=mag(1:*)
dref=dref(1:*)
snra=snra(1:*)
sndec=sndec(1:*)
pref=pref(1:*)
type=type(1:*)
;
endelse
;
return
end
