pro gx_snobs,type,survey
;
; Read the eclstat output file and output list for only type SN type and
;	survey survey type: e.g. Ia, NGS
;
if n_params(0) lt 2 then begin
	print,'GX_SNOBS Usage: gx_snobs,type,survey'
	return
endif
;
; open input file
ftyp=type
while strpos(ftyp,'/') ge 0 do strput,ftyp,'_',strpos(ftyp,'/')
openr,il,ftyp+'_ecl.out',/get_lun
rec=''
ntim=0L
nim=0L
nsn=0L
imd=strarr(1000)
subv=strarr(1000)
;
; open output file
openw,ol,ftyp+'_'+survey+'.out',/get_lun
;
; read in list file
readcol,ftyp+'_list.txt',snnm,snra,sndec,sntype,host,hra,hdec,ofew,ofns,htp,$
	htc,vz,form='a,d,d,a,a,d,d,f,f,a,f,f'
;
; read in sn coo file
readcol,'snad.dat',snnmad,sna,snd,format='a,d,d'
;
; read first record
readf,il,rec
sn=gettok(rec,' ')
postype=gettok(rec,' ')
;
; find our SN
w=where(strpos(snnm,sn) ge 0, nw)
if nw gt 0 then begin
	w=w(0)
	ohost=host(w)
	otyp=sntype(w)
;
; did we fix the sn position?
	if strpos(postype,'sn') lt 0 and snra(w) ge 0 and sndec(w) ge -90. and $
		sndec(w) le 90. then postype='sn'
endif
;
; loop over input
while not eof(il) do begin
    readf,il,rec
    if strlen(rec) lt 20 then begin
;
; process records from previous SN
        if nim gt 0 then begin
	    print,ntim,nim
	    nsn=nsn+1L
	    print,nsn,'  ',sn
            printf,ol,'#',sn,postype,ohost,otyp, $
	    	format='(a-2,a-7,a-6,a-16,a6)'
            for i=0,nim-1 do printf,ol,imd(i),'  ',subv(i),form='(a,a,a)'
        endif
	nim=0L
	imd=strarr(1000)
;
; set up for new SN
        sn=gettok(rec,' ')
	postype=gettok(rec,' ')
;
; find it
	w=where(strpos(snnm,sn) ge 0, nw)
	if nw gt 0 then begin
		w=w(0)
		ohost=host(w)
		otyp=sntype(w)
	endif
    endif else begin
;
; read in image dir list
        sta=strsplit(rec,',',/extract)
	if sta(2) eq 'd' and strpos(sta(3),survey) ge 0 and $
	   (strpos(sta(4),'PASS') ge 0 or strpos(survey,'AIS') ge 0 or $
	     strpos(survey,'CAI') ge 0 or strpos(survey,'CAS') ge 0) then begin
            	imd(nim)=sta(0)
	    	subv(nim)=string(fix(sta(1)),form='(i02)')
            	nim=nim+1L
	    	ntim=ntim+1L
	endif
    endelse
endwhile
;
; process last SN
if nim gt 0 then begin
	print,ntim,nim
	nsn=nsn+1L
	print,nsn,'  ',sn
        printf,ol,'#',sn,postype,ohost,otyp, $
		format='(a-2,a-7,a-6,a-16,a6)'
        for i=0,nim-1 do printf,ol,imd(i),'  ',subv(i),form='(a,a,a)'
endif
;
free_lun,il,ol
;
; log stats
openw,1,ftyp+'_stats.log',/append
print,'For SN type: ',type,' and survey: ',survey,' found ',nsn,' SNe in ', $
	ntim,' images.'
printf,1,'For SN type: ',type,' and survey: ',survey,' found ',nsn,' SNe in ', $
	ntim,' images.'
close,1
;
return
end
