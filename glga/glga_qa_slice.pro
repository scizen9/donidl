pro glga_qa_slice,lfile,itag,sdss=sdss,galex=galex,twomass=twomass,wise=wise, $
	silent=silent,note_string=note_string,negate=negate
;+
; glga_qa_slice - slice the list in LFILE based on QA structure tag ITAG
;
; lfile - list of objects with one line per object with these columns:
;	id
;	ra,dec	- degrees
;	majdiam,mindiam	- arcmin
;	pa	- degrees
;
; itag - input structure tag name from the qa structure (any case)
;	NOTE: leave off to interactively choose from a list of tags
;
; keywords:
;	sdss,galex,twomass,wise - data to analyze
;	silent	- supress output
;	note_string	- a string to match in the 'NOTE' tag
;	negate	- set to return inverse list (where tag value ne 1)
;-
;
; set defaults and check keywords
ddir='uv/'
fpx='galex_'
if keyword_set(sdss) then begin
	ddir='sdss/'
	fpx='sdss_'
endif
if keyword_set(galex) then begin
	ddir='uv/'
	fpx='galex_'
endif
if keyword_set(twomass) then begin
	ddir='2mass/'
	fpx='2mass_'
endif
if keyword_set(wise) then begin
	ddir='wise/'
	fpx='wise_'
endif
if keyword_set(note_string) then $
	nstr = strupcase(note_string) $
else	nstr = ''
;
; read in sample data
readcol,lfile, id, ra, dec, majdiam, mindiam, pa, type, $
	format='a,d,d,f,f,f,a', /silent
;
; define top level directory
deg = string(floor(ra), format='(i3.3)')+'D'
;
; used to generate file names
id = strcompress(id,/rem)

filebase=!GLGA_ROOT+'data/'+deg+'/'+ddir+'fits/'+id
ellfiles=!GLGA_ROOT+'data/'+deg+'/aux/'+id+'_ellipse.dat'
;
; starting index
nloop = n_elements(id)
;
; get tag names
qa=glga_read_qa_stat(filebase[0]+'_qa.txt')
names = tag_names(qa)
p=-1
if n_params(0) gt 1 then begin
	tag=strupcase(itag)
	p = where(strcmp(names,tag) eq 1, np)
endif else begin
	for i=0,n_elements(names)-1 do $
		print,i,names[i],format='(i5,2x,a)'
	while p lt 0 or p ge n_elements(names) do $
		read,'Enter tag number: ',p
	np = 1
	tag = names[p]
	itag = strlowcase(tag)
	if strcmp(tag, 'NOTE') and strlen(nstr) le 0 then $
		read,'Enter note string to match: ',nstr
	nstr = strupcase(nstr)
endelse
if np le 0 or p lt 0 then begin
	print,'Error - tag not found: ',itag
	return
endif
;
; open output file
tmp=lfile
rute=gettok(tmp,'.')
ofile=rute+'_'+fpx+strtrim(tag,2)+'.slice'
filestamp,ofile,/archdir
openw,ol,ofile,/get_lun
printf,ol,'# GLGA_QA_SLICE: '+systime(0)
if strcmp(tag, 'NOTE') and strlen(nstr) gt 0 then $
	printf,ol,'# TAG: ',strupcase(itag),' ~ ',nstr $
else	printf,ol,'# TAG: ',strupcase(itag)
if keyword_set(negate) then $
	printf,ol,'# NEGATING'
if not keyword_set(silent) then $
	print,'Writing to file: ',ofile
;
; loop over object list
count=0L
for i=0L, nloop-1 do begin
;
; print status
	if not keyword_set(silent) then $
		print,string(13B),i+1,'/',nloop,id[i],deg[i], $
		format = '($,a1,i6,a1,i6,2x,a-25,a5,a)'
;
; read QA status file
	qalogfile = filebase[i] + '_qa.txt'
	qa=glga_read_qa_stat(qalogfile)
	printit = ( 1 eq 0 )
	if qa.ts gt 0. then begin	; does qa file exist?
		if strcmp(tag, 'NOTE') eq 1 then begin	; are we checking notes?
			if strlen(nstr) gt 0 then begin
				if strpos(strupcase(qa.note),nstr) ge 0 then $
					printit = (1 eq 1)
			endif else if strlen(qa.note) gt 0 then $
					printit = (1 eq 1)
		endif else begin
			if keyword_set(negate) then begin
				if qa.(p) eq 0 then printit = (1 eq 1)
			endif else begin
				if qa.(p) ne 0 then printit = (1 eq 1)
			endelse
		endelse
	endif
	if printit then begin
		; update shape params
		if file_test(ellfiles[i]) then begin
			readcol,ellfiles[i],mjas,mnas,ral,decl,pal, $
				form='f,f,d,d,f',/sil
			mjx = mjas/60.
			mnx = mnas/60.
		endif else begin
			mjx = majdiam[i]
			mnx = mindiam[i]
			pal = pa[i]
			ral = ra[i]
			decl= dec[i]
		endelse
		printf,ol,id[i],ral,decl,mjx,mnx,pal,type[i], $
			format='(a-25,2f13.8,3f9.3,2x,a)'
		count = count + 1L
		if not keyword_set(silent) then print,' '
	endif
;
endfor	; loop over object list
free_lun,ol
;
if not keyword_set(silent) then begin
    print,string(13B),'                                                    ', $
	format='($,a1,a)'
    print,' '
    if strcmp(tag,'NOTE') eq 1 then $
    	    print,itag,' = '+nstr+', found this many: ',count,form='(a,a,i5)' $
    else    print,itag,' set, found this many: ',count,form='(a,a,i5)'
endif
;
end
