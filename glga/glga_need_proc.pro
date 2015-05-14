pro glga_need_proc,lfile,sdss=sdss,galex=galex,twomass=twomass, $
	wise=wise,silent=silent,ellipse=ellipse,dss=dss
;+
; glga_need_proc - list hosts that need processing
;
; lfile - list of objects with one line per object with these columns:
;	id
;	ra,dec	- degrees
;	majdiam,mindiam	- arcmin
;	pa	- degrees
;
; keywords:
;	sdss,galex,twomass,wise - data to analyze
;	ellipse	- set to compare phot with ellipse file
;	dss	- set to see which objects need dss data
;	silent	- supress output
;
;-
;
; set defaults and check keywords
ddir='galex/'
fpx='galex_'
flt='_NUV_'
suf='_NUV.fits.gz'
;
; GALEX data are the only ones that need dss images
if not keyword_set(dss) then begin
	if keyword_set(sdss) then begin
		ddir='sdss/'
		fpx='sdss_'
		flt='_g_'
		suf='_g.fits.gz'
	endif
	if keyword_set(galex) then begin
		ddir='galex/'
		fpx='galex_'
		flt='_NUV_'
		suf='_NUV.fits.gz'
	endif
	if keyword_set(twomass) then begin
		ddir='2mass/'
		fpx='2mass_'
		flt='_k_'
		suf='_k.fits.gz'
	endif
	if keyword_set(wise) then begin
		ddir='wise/'
		fpx='wise_'
		flt='_w1_'
		suf='_w1.fits.gz'
	endif
endif
;
; read in sample data
readcol,lfile, id, ra, dec, majdiam, mindiam, pa, typ, $
	format='a,d,d,f,f,f,a', /silent
;
; define top level directory
deg = string(floor(ra), format='(i3.3)')+'D'
;
; used to generate file names
id = strcompress(id,/rem)

filebase=!GLGA_ROOT+'data/'+deg+'/'+ddir+'fits/'+id
photbase=!GLGA_ROOT+'data/'+deg+'/photometry/'+id+flt
ellbase =!GLGA_ROOT+'data/'+deg+'/aux/'+id+'_ellipse.dat'
dssbase =!GLGA_ROOT+'data/'+deg+'/dss/fits/'+id
;
; starting index
nloop = n_elements(id)
;
; phot or ellipse?
type='needphot'
lab ='# NEEDS '+strupcase(strmid(fpx,0,strlen(fpx)-1))+' PHOTOMETRY'
if keyword_set(ellipse) then begin
	type = 'needphot_newellipse'
	lab  = lab + ' (NEW ELLIPSE)'
endif
if keyword_set(dss) then begin
	type = 'need_dss'
	lab  = '# NEEDS DSS IMAGES'
endif
;
; open output file
tmp=lfile
rute=gettok(tmp,'.')
ofile=rute+'_'+fpx+type+'.slice'
filestamp,ofile
openw,ol,ofile,/get_lun
printf,ol,'# GLGA_NEED_PROC: '+systime(0)
printf,ol,lab
if not keyword_set(silent) then $
	print,'Writing to file: ',ofile
;
; image list
ifile=rute+'_'+fpx+'noimgs.slice'
filestamp,ifile
openw,il,ifile,/get_lun
printf,il,'# GLGA_NEED_PROC: '+systime(0)
printf,il,'# NO '+ strupcase(strmid(fpx,0,strlen(fpx)-1))+' IMAGES'
;
; loop over object list
count=0L
miss =0L
for i=0L, nloop-1 do begin
;
; print status
  if not keyword_set(silent) then $
    print,string(13B),i+1,'/',nloop,id[i],deg[i], $
	format = '($,a1,i6,a1,i6,2x,a-25,a5,a)'
;
; get QA info
  if keyword_set(dss) then begin
    flist=file_search(filebase[i]+suf,count=nf)
    if nf gt 0 then begin
	dlist=file_search(dssbase[i]+'_dss*',count=df)
	if df le 0 then begin
	      printf,ol,id[i],ra[i],dec[i],majdiam[i],mindiam[i],pa[i],typ[i],$
			format='(a-25,2f13.8,3f9.3,2x,a)'
		count = count + 1L
		if not keyword_set(silent) then print,' '
	endif
    endif
  endif else begin
    flist=file_search(filebase[i]+suf,count=nf)
    if nf le 0 then begin
	printf,il,id[i],ra[i],dec[i],majdiam[i],mindiam[i],pa[i],typ[i],$
			format='(a-25,2f13.8,3f9.3,2x,a)'
	miss = miss + 1L
    endif
    qa = glga_read_qa_stat(filebase[i]+'_qa.txt')
    if qa.complete then begin
  	qinfo = file_info(filebase[i]+'_qa.txt')
  	pinfo = file_info(photbase[i]+'total.dat')
	update = (qinfo.mtime gt pinfo.mtime)
	if keyword_set(ellipse) then begin
		einfo = file_info(ellbase[i])
		update = (einfo.mtime gt pinfo.mtime)
	endif
  	if update then begin
	      printf,ol,id[i],ra[i],dec[i],majdiam[i],mindiam[i],pa[i],typ[i],$
			format='(a-25,2f13.8,3f9.3,2x,a)'
		count = count + 1L
		if not keyword_set(silent) then print,' '
	endif
    endif
  endelse
;
endfor	; loop over object list
free_lun,ol,il
;
if not keyword_set(silent) then begin
    print,string(13B),'                                                    ', $
	format='($,a1,a)'
    print,' '
    print,'Need proc - found this many: ',count,form='(a,i5)'
    print,'Missing imgs - found this many: ',miss,form='(a,i5)'
endif
;
end
