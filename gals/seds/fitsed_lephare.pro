pro fitsed_lephare, galid, glgaout=glgaout, test=test, yuan13=yuan13, $
	context=context, update=update, verbose=verbose, $
	para_file=para_file, output_para_file=output_para_file
;+
;	fitsed_lephare - fit the SED of a galaxy with lephare
;
; CALLING SEQUENCE:
;    fitsed_lephare,galid,/glgaout,test=<int>,/yuan13,/update,/verbose
;
; INPUTS:
;	galid	- galaxy id as specified in galdat struct
;
; KEYWORDS:
;	glgaout	- put in/output files in !GLGA_ROOT degree dir
;	test	- integer minimum number of valid bandpasses required to fit
;	yuan13	- use extinction coeff's of Yuan et al. 2013
;	context	- the context mask for lephare
;	update	- only fit if GLGA photometry is newer than GLGA outputs
;	para_file - full spec of lephare parameter file
;	output_para_file - full spec of lephare output parameter file
;	verbose	- extra output to screen
;
; CALLS:
;	get_bestmags to get mags to fit
;	plot_lspec if /glgaout is set
;
; HISTORY:
; $Id: fitsed_lephare.pro,v 1.22 2013/09/03 15:36:23 neill Exp $
;	05-JUN-2013, jdn - Initial Revision
;	18-JUN-2013, jdn - Implemented update keyword, good mags context
;-
common galdb_info
;
; get galaxy
if n_params(0) lt 1 then begin
	gal=''
	read,'Enter Galaxy id: ',gal
endif else gal = galid
;
; get mags
get_bestmags,gal,mags,merrs,waves,bwid,srce,gphts, $
	yuan13=yuan13,status=stat,/silent
;
; check results
if stat ne 0 then begin
	print,'Not in gal and/or glga db: ',gal
	return
endif
;
; check good mags
good = where(mags gt 0., ngood)
if keyword_set(test) then $
	ntst = test $
else	ntst = 5
if ngood lt ntst then begin
	print,'Not enough good points: ',ngood
	return
endif
;
; create context mask
cmask = 0L
for i=0,ngood-1 do begin
	if good[i] le 14 then $	; include up to W2
		cmask = cmask + 2^good[i]
endfor
;
; get galdat index
g = gfind(gal)
;
; get directories
glga_dir = !GLGA_ROOT + 'data/'+glga_degdir(galdat[g].ra)+'/seds/lephare/'
para_dir = !GALS_DATA + 'lephare/'
;
; get version string
version = '$Revision: 1.22 $'
junk = gettok(version,' ')
junk = gettok(version,' ')
version = junk
;
; context
if keyword_set(context) and not keyword_set(glgaout) then $
	cntxt = long(context) $
else	cntxt = cmask	; default to use good bands up through W2
;
; output file
if keyword_set(glgaout) then begin
	odir = glga_dir
	tail = ''
endif else begin
	odir = './'
	tail = '_'+timestr()+'_lephare'
endelse
;
; output files
magfil=odir+gal+tail + '.mags'
outfil=odir+gal+tail + '.out'
;
; check for update only
if keyword_set(update) then begin
	;
	; output timestampe
	fi = file_info(outfil)
	;
	; if photometry is older, don't fit
	if gphts le fi.mtime then begin
		if keyword_set(verbose) then $
			print,'FIT up-to-date: ',gal
		return
	endif
endif
;
; open mag file
openw,li,magfil,/get_lun
printf,li,'# FITSED_LEPHARE v'+version+': '+systime(0)
printf,li,'# Testing Mags: at least '+strn(ntst)+' must be >= 0'
printf,li,'#'

fmt='(i-8,42f9.2,i8,f7.3,2x,a-25,f9.1,f7.3,2x,a)'
;
; header
hdr='# ID         FUV      NUV      U        B        V      u_sdss   g_sdss   r_sdss   i_sdss   z_sdss     J        H        K        W1       W2       W3       W4       12mu     25mu     60mu    100mu     FUVe     NUVe     Ue       Be       Ve       usde     gsde     rsde     isde     zsde     Je       He       Ke      W1e      W2e      W3e      W4e    12mue    25mue    60mue   100mue   cntxt  zspec  Galaxy                          cz  EBVMW  SNe'
printf,li,hdr

if strlen(galdat[g].sne) le 0 then $
	sne = '-' $
else	sne = galdat[g].sne
printf,li,1L, mags, merrs, cntxt, (galdat[g].cz/!PHYS_C)>0.001, $
	gal,galdat[g].cz, galdat[g].mwebmv, sne, $
	format=fmt
;
free_lun,li
;
; parameter input and output files
;
; are we doing this for GLGA?
if keyword_set(glgaout) then begin
	pfile = odir + 'gals_zphota.para'
	opfile = odir + 'gals_zphota_output.para'
	if not file_test(pfile) then begin
		spawn,'cp '+para_dir+'gals_zphota.para '+pfile
		spawn,'cp '+para_dir+'gals_zphota_output.para '+opfile
	endif
;
; not GLGA
endif else begin
	if keyword_set(para_file) then $
		pfile = para_file $
	else	pfile = odir + 'gals_zphota.para'
	if keyword_set(output_para_file) then $
		opfile = output_para_file $
	else	opfile = odir + 'gals_zphota_output.para'
endelse
;
; output spectrum
ispec = 'Id'+string(1,format='(i09)')+'.spec'
ospec = odir + gal+tail+'.spec'
;
; zphota command
cmd = 'zphota -c '+pfile+' -CAT_IN '+magfil+' -CAT_OUT '+outfil+ $
	' -PARA_OUT '+opfile
if keyword_set(verbose) then $
	print,cmd
spawn,cmd
cmd = 'mv '+ispec+' '+ospec
if keyword_set(verbose) then $
	print,cmd
spawn,cmd
;
; check for glgaout
if keyword_set(glgaout) then $
	plot_lspec,gal,/glgaout,verbose=verbose
;
return
end
