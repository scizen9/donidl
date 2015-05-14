pro cvcatproc
;
ifil='cvcat.txt'
ofil='cvcat.dat'
filestamp,ofil
openw,ol,ofil,/get_lun
printf,ol,'#CVCATPROC - '+systime(0)
printf,ol,'#CVName1      CVName2    RA           DEC         Type1    Type2     Mag1    Mag2    Mag3    Mag4    T1     T2     Per1   Per2   Per3    Per4    EB    SB    SpCl2   SpCl1   M1/M2   dM1/M2   Incl    dIncl    M1     dM1    M2    dM2'
;
rec = ''
openr,il,ifil,/get_lun
for i=0,5 do readf,il,rec
while not eof(il) do begin
	cvnm1='-'
	cvnm2='-'
	ra=-9.d0
	dec=0.d0
	ty1='-'
	ty2='-'
	ty3='-'
	ty4='-'
	mg1=-99.99
	mg2=-99.99
	mg3=-99.99
	mg4=-99.99
	t1=-9
	t2=-9
	p1=-99.99
	p2=-99.99
	p3=-99.99
	p4=-99.99
	eb=-9
	sb=-9
	sp1='-'
	sp2='-'
	mr=-99.99
	dmr=-9.99
	inc=-99.99
	dinc=-9.99
	m1=-99.99
	dm1=-9.99
	m2=-99.99
	dm2=-9.99
	readf,il,rec
	if strpos(rec,'------') ge 0 then begin
		printf,ol,cvnm1,cvnm2,ra,dec,ty1,ty2,ty3,ty4,mg1,mg2,mg3,mg4, $
			t1,t2,p1,p2,p3,p4,eb,sb,sp1,sp2,mr,dmr,inc,dinc, $
			m1,dm1,m2,dm2, $
	format='(a-14,a-14,2f13.8,2x,4a-4,4f7.1,2i7,f9.6,f9.2,f9.6,f9.2,2i4,)'
	endif else begin
		sta=strsplit(rec,'|',/extract)
		if strpos(rec,'*') ge 0 then begin
			cvnm1=strcompress(sta[0])
			sra = strtrim(sta[2],2)
			rad = fix(gettok(sra,' '))
			ram = fix(gettok(sra,' '))
			ras = sra
			ra  = ten(rad,ram,ras)*15.d0
			ty1 = strtrim(sta[3],2)
			ty3 = strtrim(sta[4],2)
		endif else begin
		endelse
	endelse
endwhile
;
return
end
