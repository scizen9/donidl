pro adoffset,ra0,dec0,dra,ddec,u,ra1,dec1
;+
;	ADOFFSET - apply offset to coords
;
; USAGE:
;
;	adoffset,ra0,dec0,dra,ddec,u,ra1,dec1
;
; INPUTS:
;	ra0,dec0 - input coords
;	dra,ddec - offset to apply
;	u	 - offset units: 0 - arcsec, 1 - arcmin, 2 - degrees
;
; OUTPUTS:
;
;	ra1,dec1 - the new coords
;
; NOTE:
;
;	Assumes coords in decimal degrees
;
; HISTORY:
;
;	071019 jdn - initial revision
;-
;
if n_params(0) lt 5 then begin
	print,'Usage: adoffset,ra0,dec0,dra,ddec,u,ra1,ra2'
	return
endif
;
case u of
	0: begin
		dec1 = dec0 + ddec / 3600.d0
		ddra = dra / cos( avg([dec0,dec1])/!radeg )
		ra1  = ra0 + ddra / 3600.d0
		end
	1: begin
		dec1 = dec0 + ddec / 60.d0
		ddra = dra / cos( avg([dec0,dec1])/!radeg )
		ra1  = ra0 + ddra / 60.d0
		end
	2: begin
		dec1 = dec0 + ddec
		ddra = dra / cos( avg([dec0,dec1])/!radeg )
		ra1  = ra0 + ddra
		end
	else: print,'ADOFFSET - Error: u must be 0,1 or 2: ',u
endcase
;
return
end
