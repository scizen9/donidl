; $Id: tf_coeffs.pro,v 1.2 2014/05/08 16:14:22 neill Exp $
pro tf_coeffs,ffile,tfzp,tfzpe,tfsl,tfsle,tfcv,tfcve
;+
; tf_coeffs - read the coefficients for the TFR from FFILE
;-
if strpos(ffile,'tffits') ge 0 then begin
	readcol,ffile,name,ng,zp,j1,zpe,sl,j2,sle,form='a,i,f,a,f,f,a,f',/silent
	tfcv = 0.
	tfcve = 0.
endif else begin
	readcol,ffile,name,ng,zp,j1,zpe,sl,j2,sle,cv,j3,cve, $
		form='a,i,f,a,f,f,a,f,f,a,f',/silent
	t = where(strpos(name,'_ZP') ge 0, nt)
	tfcv = cv[t[0]]
	tfcve = cve[t[0]]
endelse
;
; zero point
t = where(strpos(name,'_ZP') ge 0, nt)
if nt ge 1 then begin
	tfzp = zp[t[0]]
	tfzpe = zpe[t[0]]
	tfsl = sl[t[0]]
	tfsle = sle[t[0]]
endif else begin
	print,'ERROR - ZP fit not found'
	tfzp = 0.
	tfzpe = 0.
	tfsl = 1.
	tfsle = 0.
endelse

return
end
