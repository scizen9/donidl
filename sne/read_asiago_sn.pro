pro read_asiago_sn,sn,host,hra,hdec,snra,sndec,gtyp,gtc,ginc,gpa,vz,gbt,glgd25,$
		ofew,ofns,snfilt,snmag,mty,type,ddate,disc,silent=silent,$
		reread=reread
;
afile=!SNE_DATA+'/asiago.dat'
if not keyword_set(reread) and file_test(afile,/read) then begin
	readcol,afile,sn,host,hra,hdec,snra,sndec, $
		gtyp,gtc,ginc,gpa,vz,gbt,glgd25,ofew,ofns, $
		snfilt,snmag,mty,type,ddate,disc,silent=silent, $
		format='a,a,d,d,d,d,a,f,f,f,f,f,f,f,f,a,f,a,a,a,a'
	n=n_elements(sn)
endif else begin
;
; move original
filestamp,afile,/arch,/verbose
;
; setups
sn=['']		; SN ID
host=['']	; host
hra=[0.d]	; host RA
hdec=[0.d]	; host Dec
snra=[0.d]	; SN RA
sndec=[0.d]	; SN Dec
gtyp=['']	; host type
gtc=[0.]	; host type code (RC3)
ginc=[0.]	; host inclination (degrees: 0 - face on)
gpa=[0.]	; host position angle (degrees north eastwards)
vz=[0.]		; either redshift (z > 0.1) or velocity kms
gbt=[0.]	; host integrated B mag
glgd25=[0.]	; decimal log of isophotal diameter in 0.1 arcmin units
ofew=[0.]	; asec offset (+E, -W)
ofns=[0.]	; asec offset (+N, -S)
snfilt=['']	; SN mag filter
snmag=[0.]	; SN mag
mty=['']	; SN mag type (* - discovery mag, otherwise max)
type=['']	; SN type
ddate=['']	; SN epoch of max ('*' marks date of discovery)
disc=['']	; name of discoverer
;
tfile=!SNE_DATA+'/asiago.txt'
openr,il,tfile,/get_lun
;
while not eof(il) do begin
	rec=' '
	readf,il,rec
	if strlen(strtrim(rec,2)) gt 1 and strpos(rec,'DISCOVERER') lt 0 and $
		strpos(rec,'1990 al?') lt 0 then begin	; skip evil 1990al?
		sn=[sn,strcompress(strmid(rec,2,8),/remove)]
		host=[host,strcompress(strmid(rec,12,18),/remove)]
		if strtrim(strmid(rec,32,7),2) eq '' then $
			hra=[hra,-9.d0] $
		else	if strpos(strmid(rec,32,7),'.') ge 0 then $
			hra=[hra,ten(fix(strmid(rec,32,2)), $
				double(strmid(rec,34,4)))*15.d0] $
		else	hra=[hra,ten(fix(strmid(rec,32,2)), $
			fix(strmid(rec,34,2)),double(strmid(rec,36,2)))*15.d0]
		if strtrim(strmid(rec,42,8),2) eq '' then $
			hdec=[hdec,-99.d0] $
		else	begin
			tdec=ten(fix(strmid(rec,43,2)), $
			fix(strmid(rec,45,2)), $
			double(strtrim(strmid(rec,47,2),2)))
			if strmid(rec,42,1) eq '-' then tdec=-tdec
			hdec=[hdec,tdec]
		endelse
		if strtrim(strmid(rec,52,10),2) eq '' then $
			snra=[snra,-9.d0] $
		else	snra=[snra,ten(fix(strmid(rec,52,2)), $
			fix(strmid(rec,54,2)),double(strmid(rec,56,5)))*15.d0]
		if strtrim(strmid(rec,62,10),2) eq '' then $
			sndec=[sndec,-99.d0] $
		else	begin
			tdec=ten(fix(strmid(rec,63,2)), $
			fix(strmid(rec,65,2)),double(strmid(rec,67,4)))
			if strmid(rec,62,1) eq '-' then tdec=-tdec
			sndec=[sndec,tdec]
		endelse
		if strtrim(strmid(rec,77,9),2) eq '' then $
			gtyp=[gtyp,'-'] $
		else	gtyp=[gtyp,strcompress(strmid(rec,77,9),/remove)]
		if strtrim(strmid(rec,87,8),2) eq '' then $
			gtc=[gtc,-99.9] $
		else	gtc=[gtc,float(strmid(rec,87,8))]
		if strtrim(strmid(rec,97,8),2) eq '' then $
			ginc=[ginc,-99.9] $
		else	ginc=[ginc,float(strmid(rec,97,8))]
		if strtrim(strmid(rec,107,8),2) eq '' then $
			gpa=[gpa,-99.9] $
		else	gpa=[gpa,float(strmid(rec,107,8))]
		if strtrim(strmid(rec,117,8),2) eq '' then $
			vz=[vz,-9999.9] $
		else	vz=[vz,float(gettok(strmid(rec,117,8),':'))]
		if strtrim(strmid(rec,128,8),2) eq '' then $
			gbt=[gbt,-99.9] $
		else	gbt=[gbt,float(gettok(strmid(rec,128,8),':'))]
		if strtrim(strmid(rec,137,8),2) eq '' then $
			glgd25=[glgd25,-99.9] $
		else	glgd25=[glgd25,float(strmid(rec,137,8))]
		if strtrim(strmid(rec,147,8),2) eq '' then $
			ofew=[ofew,-9999.9] $
		else	begin
			val=float(strmid(rec,147,8))
			if strpos(strmid(rec,147,8),'W') ge 0 then $
				sig=-1.0 $
			else	sig=1.0
			ofew=[ofew,val*sig]
		endelse
		if strtrim(strmid(rec,157,8),2) eq '' then $
			ofns=[ofns,-9999.9] $
		else	begin
			val=float(strmid(rec,157,8))
			if strpos(strmid(rec,157,8),'S') ge 0 then $
				sig=-1.0 $
			else	sig=1.0
			ofns=[ofns,val*sig]
		endelse
		if strtrim(strmid(rec,167,1),2) eq '' then $
			snfilt=[snfilt,'-'] $
		else	snfilt=[snfilt,strmid(rec,167,1)]
		snmag=[snmag,float(strtrim(strmid(rec,168,5),2))]
		if strmid(rec,172,1) eq '*' or strmid(rec,172,1) eq ':' then $
			mty=[mty,strmid(rec,172,1)] $
		else	mty=[mty,'-']
		if strtrim(strmid(rec,177,8),2) eq '' then $
			type=[type,'-'] $
		else	type=[type,strcompress(strmid(rec,177,8),/remove)]
		if strtrim(strmid(rec,187,8),2) eq '' then $
			ddate=[ddate,'-'] $
		else	ddate=[ddate,strcompress(strmid(rec,187,8),/remove)]
		disc=[disc,strcompress(strmid(rec,196,50),/remove)]
	endif
endwhile
;
free_lun,il
;
sn=sn(1:*)
host=host(1:*)
hra=hra(1:*)
hdec=hdec(1:*)
snra=snra(1:*)
sndec=sndec(1:*)
gtyp=gtyp(1:*)
gtc=gtc(1:*)
ginc=ginc(1:*)
gpa=gpa(1:*)
vz=vz(1:*)
gbt=gbt(1:*)
glgd25=glgd25(1:*)
ofew=ofew(1:*)
ofns=ofns(1:*)
snfilt=snfilt(1:*)
snmag=snmag(1:*)
mty=mty(1:*)
type=type(1:*)
ddate=ddate(1:*)
disc=disc(1:*)
n=n_elements(sn)
;
openw,ol,afile,/get_lun
for i=0,n-1 do $
	printf,ol,sn(i),host(i),hra(i),hdec(i),snra(i),sndec(i),gtyp(i),gtc(i),$
		ginc(i),gpa(i),vz(i),gbt(i),glgd25(i),ofew(i),ofns(i), $
		snfilt(i),snmag(i),mty(i),type(i),ddate(i),disc(i), $
		format='(a-10,a-15,4f13.8,1x,a-7,f6.1,2f7.0,f10.3,1x,f6.2,f7.2,2f8.1,a4,f6.2,1x,a1,1x,a-8,1x,a-10,2x,a)'
free_lun,ol
endelse
;
return
end
