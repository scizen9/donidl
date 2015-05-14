pro read_lspec,sfile,galwav,galmag, $
	firwav=firwav,firmag=firmag, $
	obmag=obmag,oberr=oberr,flwav=flwav,flwid=flwid, $
	gpmag=gpmag,fpmag=fpmag, $
	idno=idno,zspec=zspec,zphot=zphot,nfilt=nfilt,pdf=pdf, $
	npgal=npgal,mdgal=mdgal,ligal=ligal,nbgal=nbgal,chgal=chgal, $
	exgal=exgal,ebgal=ebgal, $
	aggal=aggal,msgal=msgal,sfgal=sfgal,ssgal=ssgal, $
	npfir=npfir,mdfir=mdfir,lifir=lifir,nbfir=nbfir,chfir=chfir, $
	lmfir=lmfir,verbose=verbose,status=status
;+
;	read_lspec - read in LePhare *.spec files
;
; CALLING SEQUENCE:
;	read_lspec,sfile,wave,fit
;
; INPUTS:
;	sfile	- spec file name
;
; OUTPUTS:
;	galwav	- wavelengths
;	galmag	- magnitudes
;
; HISTORY:
;	28-jul-2011, jdn - Initial Version
;	20-nov-2013, jdn - Added status keyword
;-
status = -1
if not file_test(sfile) then begin
	print,'File not found: ',sfile
	return
endif
;
openr,il,sfile,/get_lun
rec=''
readf,il,rec	; header
readf,il,rec	; first record
idno = long(gettok(rec,' '))
zspec = float(gettok(rec,' '))
zphot = float(gettok(rec,' '))
;
readf,il,rec	; header
readf,il,rec	; second record
jnk = gettok(rec,' ')
nfilt = fix(gettok(rec,' '))
;
readf,il,rec	; header
readf,il,rec	; third record
jnk = gettok(rec,' ')
pdf = fix(gettok(rec,' '))
;
readf,il,rec	; header
readf,il,rec	; GAL-1 record
jnk = gettok(rec,' ')
npgal = fix(gettok(rec,' '))	; points in galaxy model
mdgal = fix(gettok(rec,' '))	; galaxy library model number
ligal = fix(gettok(rec,' '))	; library number
nbgal = fix(gettok(rec,' '))	; number of bands used
for i=0,2 do jnk = gettok(rec,' ')	; skip next items
chgal = float(gettok(rec,' '))	; chi^2 of model fit
jnk = gettok(rec,' ')		; skip next item
exgal = fix(gettok(rec,' '))	; extinction law
ebgal = float(gettok(rec,' '))	; E(B-V)
jnk = gettok(rec,' ')		; skip next item
aggal = float(gettok(rec,' '))	; Log Age (yrs)
msgal = float(gettok(rec,' '))	; Log Mass (Msun)
sfgal = float(gettok(rec,' '))	; Log SFR (Msun/yr)
ssgal = float(gettok(rec,' '))	; Log sSFR (1/yr)
readf,il,rec	; GAL-2 record
readf,il,rec	; GAL-FIR record
jnk = gettok(rec,' ')
npfir = fix(gettok(rec,' '))	; points in galaxy model
mdfir = fix(gettok(rec,' '))	; galaxy library model number
lifir = fix(gettok(rec,' '))	; library number
nbfir = fix(gettok(rec,' '))	; number of bands used
for i=0,2 do jnk = gettok(rec,' ')	; skip next items
chfir = float(gettok(rec,' '))	; chi^2 of model fit
for i=0,2 do jnk = gettok(rec,' ')	; skip next items
lmfir = float(gettok(rec,' '))	; Log FIR Lum (Lsun)
readf,il,rec	; GAL-STOCH record
readf,il,rec	; QSO record
readf,il,rec	; STAR record
;
; observed AB mags and predicted AB mags from SED fit
obmag = fltarr(nfilt)
oberr = fltarr(nfilt)
flwav = fltarr(nfilt)	; filter wavelengths (Angstroms)
flwid = fltarr(nfilt)	; filter widths (Angstroms)
gpmag = fltarr(nfilt)	; galaxy predicted mags
fpmag = fltarr(nfilt)	; FIR predicted mags
for i=0,nfilt-1 do begin
	readf,il,rec
	rec = strtrim(rec,2)
	obmag[i] = float(gettok(rec,' '))
	oberr[i] = float(gettok(rec,' '))
	flwav[i] = float(gettok(rec,' '))
	flwid[i] = float(gettok(rec,' '))
	gpmag[i] = float(gettok(rec,' '))
	fpmag[i] = float(gettok(rec,' '))
endfor
;
; galaxy SED model
if npgal gt 0 then begin
	galwav = fltarr(npgal)
	galmag = fltarr(npgal)
	for i=0,npgal-1 do begin
		readf,il,rec
		rec = strtrim(rec,2)
		galwav[i] = float(gettok(rec,' '))
		galmag[i] = float(gettok(rec,' '))
	endfor
endif else begin
	galwav = -1
	galmag = -1
endelse
;
; FIR SED model
if npfir gt 0 then begin
	firwav = fltarr(npfir)
	firmag = fltarr(npfir)
	for i=0,npfir-1 do begin
		readf,il,rec
		rec = strtrim(rec,2)
		firwav[i] = float(gettok(rec,' '))
		firmag[i] = float(gettok(rec,' '))
	endfor
endif else begin
	firwav = -1
	firmag = -1
endelse
;
if keyword_set(verbose) then $
	print,sfile+' ng1: ',npgal,' nfir: ',npfir
;
free_lun,il
status = 0
;
return
end
