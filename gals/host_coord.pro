pro host_coord,name,rad,decd,verbose=verbose
a=0
;+
;	host_coord - extract coordinates from host name
;-
rad=-99.99
decd=-99.99
;
digits = '0123456789'
;
; trim front
temp = name
while strpos(digits,strmid(temp,0,1)) lt 0 do $
	temp = strmid(temp,1)
;
; trim back
while strpos(digits,strmid(temp,strlen(temp)-1)) lt 0 do $
	temp = strmid(temp,0,strlen(temp)-1)
;
; Dec sign and split
if strpos(temp,'-') ge 0 then begin
	sgn = -1.d0
	ra = gettok(temp,'-')
endif else begin
	sgn =  1.d0
	ra = gettok(temp,'+')
endelse
dec= temp
;
; process RA
rp = strpos(ra,'.')
case rp of
	-1: begin
		rah = double(strmid(ra,0,2))
		ra = strmid(ra,2)
		if strlen(ra) gt 0 then $
			ram = double(strmid(ra,0,2)) $
		else	ram = 0.d0
		ra = strmid(ra,2)
		if strlen(ra) gt 0 then $
			ras = double(strmid(ra,0,2)) $
		else	ras = 0.d0
		rad = 15.d0 * (rah + ram/60.d0 + ras/3600.d0)
	   end
	2: rad = 15.d0 * double(ra)
	4: begin
		rah = double(strmid(ra,0,2))
		ram = double(strmid(ra,2))
		rad = 15.d0 * (rah + ram/60.d0)
	   end
	6: begin
		rah = double(strmid(ra,0,2))
		ram = double(strmid(ra,2,2))
		ras = double(strmid(ra,4))
		rad = 15.d0 * (rah + ram/60.d0 + ras/3600.d0)
	   end
	else: print,'HOST_COORD: Error - RA mal-formed: ',ra
endcase
;
; process Dec
dp = strpos(dec,'.')
case dp of
	-1: begin
		ded = double(strmid(dec,0,2))
		dec = strmid(dec,2)
		if strlen(dec) gt 0 then $
			dem = double(strmid(dec,0,2)) $
		else	dem = 0.d0
		dec = strmid(dec,2)
		if strlen(dec) gt 0 then $
			des = double(strmid(dec,0,2)) $
		else	des = 0.d0
		decd = ded + dem/60.d0 + des/3600.d0
	   end
	2: decd = double(dec)
	4: begin
		ded = double(strmid(dec,0,2))
		dem = double(strmid(dec,2))
		decd = ded + dem/60.d0
	   end
	6: begin
		ded = double(strmid(dec,0,2))
		dem = double(strmid(dec,2,2))
		des = double(strmid(dec,4))
		decd = ded + dem/60.d0 + des/3600.d0
	   end
	else: print,'HOST_COORD: Error - Dec mal-formed: ',dec
endcase
;
; apply Dec sign
decd = decd * sgn
;
if keyword_set(verbose) then begin
	ras=strtrim(string(rad,form='(f13.8)'),2)
	des=strtrim(string(decd,form='(f13.8)'),2)
	print,name,': ',ras,',',des
endif
;
return
end
