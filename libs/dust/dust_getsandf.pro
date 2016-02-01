pro dust_getsandf,ra,dec,aju=aju,ajb=ajb,ajv=ajv,ajr=ajr,aji=aji, $
	asu=asu,asg=asg,asr=asr,asi=asi,asz=asz,auj=auj,auh=auh,auk=auk,aul=aul
;+
;	dust_get - get Schlafly & Finkbiner A(filt) values from NED
; INPUTS:
;	ra,dec	- coordinates in decimal degrees
; KEYWORDS:
;	aju	- A for Landolt U-band
;	ajb	- A for Landolt B-band
;	ajv	- A for Landolt V-band
;	ajr	- A for Landolt R-band
;	aji	- A for Landolt I-band
;	asu,asg,asr,asi,asz	- A for SDSS ugriz bands
;	auj,auh,auk,aul		- A for Ukirt JHKL bands
;-
; parse ra,dec
rec = adstring(ra,dec)
;
; query parts
qp1 = 'wget -O - "http://ned.ipac.caltech.edu/cgi-bin/calc?in_csys=Equatorial&in_equinox=J2000.0&obs_epoch=2000.0&lon='
hc = '%3A'
qp2 = '&lat='
qp3 = '&pa=0.0&out_csys=Equatorial&out_equinox=J2000.0"'
;
; extract coordinate parts
rah = gettok(rec,' ')
ram = gettok(rec,' ')
ras = gettok(rec,' ')
ddg = gettok(rec,' ')
dmn = gettok(rec,' ')
dsc = gettok(rec,' ')
;
; construct query
qstr = qp1 + rah + hc + ram + hc + ras + qp2 + $
	ddg + hc + dmn + hc + dsc + qp3
;
; execute it
spawn,qstr,stdout,stderr
;
; extract results
;
; Landolt U
t=where(strpos(stdout,'Landolt U') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	aju = float(sta[3])
endif else aju = -999.
;
; Landolt B
t=where(strpos(stdout,'Landolt B') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	ajb = float(sta[3])
endif else ajb = -999.
;
; Landolt V
t=where(strpos(stdout,'Landolt V') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	ajv = float(sta[3])
endif else ajv = -999.
;
; Landolt R
t=where(strpos(stdout,'Landolt R') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	ajr = float(sta[3])
endif else ajr = -999.
;
; Landolt I
t=where(strpos(stdout,'Landolt I') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	aji = float(sta[3])
endif else aji = -999.
;
; SDSS    u
t=where(strpos(stdout,'SDSS    u') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	asu = float(sta[3])
endif else asu = -999.
;
; SDSS    g
t=where(strpos(stdout,'SDSS    g') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	asg = float(sta[3])
endif else asg = -999.
;
; SDSS    r
t=where(strpos(stdout,'SDSS    r') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	asr = float(sta[3])
endif else asr = -999.
;
; SDSS    i
t=where(strpos(stdout,'SDSS    i') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	asi = float(sta[3])
endif else asi = -999.
;
; SDSS    z
t=where(strpos(stdout,'SDSS    z') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	asz = float(sta[3])
endif else asz = -999.
;
; UKIRT   J
t=where(strpos(stdout,'UKIRT   J') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	auj = float(sta[3])
endif else auj = -999.
;
; UKIRT   H
t=where(strpos(stdout,'UKIRT   H') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	auh = float(sta[3])
endif else auh = -999.
;
; UKIRT   K
t=where(strpos(stdout,'UKIRT   K') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	auk = float(sta[3])
endif else auk = -999.
;
; UKIRT   L'
t=where(strpos(stdout,'UKIRT   L') ge 0, nt)
if nt gt 0 then begin
	sval = stdout[t[0]]
	sta = strsplit(sval,' ',/extract)
	aul = float(sta[2])
endif else aul = -999.
;
return
end
