pro snprofan,snid,host,band,path,sb,sbe,lfrac,pdel,pdele
;+
; snprofan - analyze profile at position of SN
;-
common sndb_info
;
; read GLGA profile data
read_radprof,host,band,a,tmg,tmge,amg,amge,cra,cdec,smaj,smin, $
	pathtoprofile=path
;
; did we get profile data?
if a[0] lt 0. then begin
	lfrac=-9.
	pdel=-99.
	pdele=-9.
	return
endif
;
; find SN isophotal radius
w=snfind(strtrim(snid,2))
asn= sndat[w].r_iso
;
; first get fraction of light interior to SN
zp = 25.0	; arbitrary because we are calculating fraction of light
;
; is SN inside galaxy?
if asn lt smaj[0] then begin
;
; profile any good?
	good = where(tmg gt 0., ngood)
	if ngood gt 1 then begin
;
; get total light within smaj
		mxmg = interpol(tmg[good],a[good],smaj[0])
		tlight = 10.^(-0.4*(mxmg-zp))
;
; get light interior to SN
		rmagn = interpol(tmg[good],a[good],asn[0])
		slight = 10.^(-0.4*(rmagn-zp))
;
; fraction of light within SN position
		lfrac = ( slight / tlight ) < 1.
;
; test and replace if bad
		if not finite(lfrac) or lfrac lt 0. then lfrac = -9.
;
; no good
	endif else lfrac = -9.
;
; off profile so all light is interior
endif else lfrac = 1.
;
; now get profile offset
good = where(amg gt 0., ngood)
if ngood gt 1 and sb gt 0. and sb lt 40. then begin
	pmag = interpol(amg[good],a[good],asn)
	pmge = interpol(amge[good],a[good],asn)
	pdel = sb - pmag
	pdele= sqrt(sbe^2 + pmge^2) < 99.99
;
; no good
endif else begin
	pdel = -99.0
	pdele= -9.0
endelse
;
return
end
