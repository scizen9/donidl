pro glga_magtable,lfile,sdss=sdss,galex=galex,twomass=twomass,wise=wise, $
	test=test, verbose=verbose
;+
; glga_magtable - create a table of total (asymptotic) magnitudes
;
; lfile - the GLGA standard input file with the following columns:
;	id, ra, dec, majoraxis, minoraxis, pa, type
;	with ra, dec in decimal degrees,
;	major and minor axes in arcminutes
;	pa in degrees
;-
; set defaults and check keywords
filts = ['FUV','NUV','u','g','r','i','z','j','h','k','w1','w2','w3','w4']
if keyword_set(galex) then begin
	filts = ['FUV','NUV']
endif
if keyword_set(sdss) then begin
	filts = ['u','g','r','i','z']
endif
if keyword_set(twomass) then begin
	filts = ['j','h','k']
endif
if keyword_set(wise) then begin
	filts = ['w1','w2','w3','w4']
endif
nfilts = n_elements(filts)
;
; check test
if keyword_set(test) then $
	tstn = test $
else	tstn = 0
;
; check input file
if not file_test(lfile) then begin
	print,'GLGA_MAGTABLE: Error - file not found: ',lfile
	return
endif
;
; read in sample data
readcol,lfile,id,ra,dec,major,minor,pa,type,form='a,d,d,f,f,f,a', $
	comment='#',/silent
nobj = n_elements(ra)
id = strcompress(id,/rem)
type = strcompress(type,/rem)
;
; define top level directory
deg = string(floor(ra), format='(i3.3)')+'D'
filebase=!GLGA_ROOT+'data/'+deg+'/photometry/'+id
plotbase=!GLGA_ROOT+'data/'+deg+'/plots/'+id
;
; open table file
temp = lfile
rute = gettok(temp,'.')
ofile = rute + '.tab'
filestamp,ofile,/arch
openw,ol,ofile,/get_lun
printf,ol,'# GLGA_MAGTABLE: '+systime(0)
printf,ol,'# Input list: '+lfile
printf,ol,"# ID                          RA           Dec          Maj'    Min'  PAdeg  Type            FUVr'    FUV      FUVe     NUVr'    NUV      NUVe     ur'       u       ue       gr'       g       ge       rr'       r       re       ir'       i       ie       zr'       z       ze       jr'       j       je       hr'       h       he       kr'       k       ke       w1r'     w1       w1e      w2r'     w2       w2e      w3r'     w3       w3e      w4r'     w4       w4e"
;
; set format
fmt = '(a-25,2x,2f13.8,2f8.3,f6.1,2x,a-10,2x,'+strn(3*nfilts)+'f9.3)'
;
; loop over objects
for i=0,nobj-1 do begin
	;
	; init output vars
	data = fltarr(3*nfilts) - 9.99
	p = 0l
	nread = 0l
	;
	; print status
	if keyword_set(verbose) then $
		print,i,'/',nobj,id[i],deg[i],'  ', $
			format='($,i7,a1,i7,2x,a-25,2x,a,a)'
	;
	; loop over filters
	for f=0,nfilts-1 do begin
		;
		; test photometry file
		asymfile = filebase[i]+'_'+filts[f]+'_asymptotic.dat'
		gradfile = plotbase[i]+'_'+filts[f]+'_grade?'
		if file_test(asymfile) then begin
			gflist = file_search(gradfile,count=ngf)
			if ngf gt 0 then $
				grade = fix(strmid(gflist[0], $
					strpos(gflist[0],'grade')+5,1)) $
			else	grade = 1
			if grade le 2 then begin
				if keyword_set(verbose) then $
					print,filts[f],format='($,a-4)'
				readcol,asymfile,asyma,asymag,asymag_e,/silent
				data[p] = asyma/60.	& p += 1
				data[p] = asymag	& p += 1
				data[p] = asymag_e	& p += 1
				nread += 1
			endif else p = p + 3
		endif else p = p + 3
	endfor
	if keyword_set(verbose) then $
		print,' '
	;
	; print it
	if nread ge tstn then $
	    printf,ol,id[i],ra[i],dec[i],major[i],minor[i],pa[i],type[i],data, $
		format=fmt
endfor
;
free_lun,ol
;
return
end
