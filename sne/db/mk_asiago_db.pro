pro mk_asiago_db, silent=silent
;
; create sn.dump file to use to load asncat
;
; files
afile=!SNE_DATA+'asiago.dat'
ofile=!SNE_DATA+'db/asncat/sn.dump'
;
; read text catalog
if file_test(afile,/read) then begin
	readcol,afile,sn,host,hra,hdec,snra,sndec, $
		gtyp,gtc,ginc,gpa,vz,gbt,glgd25,ofew,ofns, $
		snfilt,snmag,mty,type,ddate,disc,silent=silent, $
		format='a,a,d,d,d,d,a,f,f,f,f,f,f,f,f,a,f,a,a,a,a'
;
; calculate cz
	cz=vz
	t=where(vz gt 0. and vz lt 2.)
	cz(t) = vz(t) * !phys_c
;
; calculate d25
	d25 = 10.^(glgd25) * 6.	; to arcsec
;
; open dump file
	filestamp,ofile
	openw,ol,ofile,/get_lun
;
; preamble
	printf,ol,'--'
	printf,ol,'-- Asiago SN Catalog'
	printf,ol,'-- Created by mk_asiago_db on '+systime(0)
	printf,ol,'--'
	printf,ol,"SET client_encoding = 'UTF8';"
	printf,ol,'SET check_function_bodies = false;'
	printf,ol,'SET client_min_messages = warning;'
	printf,ol,'SET search_path = sn, pg_catalog;'
	printf,ol,' '
	printf,ol,'COPY sn (id, "name", "host", hra, hdec, ra, dec, "gtype",'+$
	    ' gtc, ginc, gpa, gbt, d25, cz, off_ew, off_ns, snmag,'+$
	    ' "snfilt", "mtyp", "sntype", "ddate", "discoverer") FROM stdin;'
	n=n_elements(sn)
	t="	"
	for i=0,n-1 do begin
		id = strmid(sn(i),0,4) + '.' + $
			strlowcase(strtrim(strmid(sn(i),4,2)))
		orec=id
		orec=orec+t+strtrim(sn(i),2)
		orec=orec+t+strtrim(host(i),2)
		orec=orec+t+strtrim(string(hra(i),form='(f13.8)'),2)
		orec=orec+t+strtrim(string(hdec(i),form='(f13.8)'),2)
		orec=orec+t+strtrim(string(snra(i),form='(f13.8)'),2)
		orec=orec+t+strtrim(string(sndec(i),form='(f13.8)'),2)
		orec=orec+t+strtrim(gtyp(i),2)
		orec=orec+t+strtrim(string(gtc(i),form='(f7.1)'),2)
		orec=orec+t+strtrim(string(ginc(i),form='(f7.1)'),2)
		orec=orec+t+strtrim(string(gpa(i),form='(f7.1)'),2)
		orec=orec+t+strtrim(string(gbt(i),form='(f7.1)'),2)
		orec=orec+t+strtrim(string(d25(i),form='(f7.1)'),2)
		orec=orec+t+strtrim(string(cz(i),form='(f9.1)'),2)
		orec=orec+t+strtrim(string(ofew(i),form='(f7.1)'),2)
		orec=orec+t+strtrim(string(ofns(i),form='(f7.1)'),2)
		orec=orec+t+strtrim(string(snmag(i),form='(f7.1)'),2)
		orec=orec+t+strtrim(snfilt(i),2)
		orec=orec+t+strtrim(mty(i),2)
		orec=orec+t+strtrim(type(i),2)
		orec=orec+t+strtrim(ddate(i),2)
		orec=orec+t+strtrim(disc(i),2)
		printf,ol,orec
	endfor
	if not keyword_set(silent) then $
		print,n,' records processed'
;
; close dump file
	free_lun,ol
endif else begin
	print,'MK_ASIAGO_DB: Error - file not found: ',afile
	return
endelse
;
return
end
