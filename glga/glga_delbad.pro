pro glga_delbad,ilst,galex=galex,sdss=sdss,twomass=twomass,wise=wise, $
	ellipse=ellipse,qa=qa,build=build
;+
;	glga_delbad - delete bad data from GLGA
;
;	ilst	- list of hosts to delete, must have id and ra one per line
;
;	set keyword for data type
;	ellipse	- set to delete ellipse file
;	qa	- set to delete qa file
;	build	- set to delete build data
;
; 	writes a csh script for deleting glga files and another script for
;	deleting mosaic build data
;-
if keyword_set(galex) then begin
	dty= 'uv/'
	a1 = '_ellipse.dat*'
	a2 = '_mask.dat*'
	a3 = '_pointsrc.dat*'
	a4 = '_roi.dat*'
	ph = '_?UV_*.dat'
	pi = '_images.*'
	pp = '_profile.*'
	im = '-?d-*.fit*'
	qf = '_qa.txt'
	jp = '_?d_2color.jpg'
	tl = '_galex_rm'
endif
if keyword_set(sdss) then begin
	dty= 'sdss/'
	a1 = '_ellipse.dat*'
	a2 = '_sdss_mask.dat*'
	a3 = '_sdss_pointsrc.dat*'
	a4 = '_sdss_roi.dat*'
	ph = '_[ugriz]_*.dat'
	pi = '_sdss_images.*'
	pp = '_sdss_profile.*'
	im = '_[ugriz]*.fit*'
	qf = '_qa.txt'
	jp = '_[ugrizwx].jpg'
	tl = '_sdss_rm'
endif
if keyword_set(twomass) then begin
	dty= '2mass/'
	a1 = '_ellipse.dat*'
	a2 = '_2mass_mask.dat*'
	a3 = '_2mass_pointsrc.dat*'
	a4 = '_2mass_roi.dat*'
	ph = '_[jhk]_*.dat'
	pi = '_2mass_images.*'
	pp = '_2mass_profile.*'
	im = '_[jhk]*.fit*'
	qf = '_qa.txt'
	jp = '_[jhkx].jpg'
	tl = '_2mass_rm'
endif
if keyword_set(wise) then begin
	dty= 'wise/'
	a1 = '_ellipse.dat*'
	a2 = '_wise_mask.dat*'
	a3 = '_wise_pointsrc.dat*'
	a4 = '_wise_roi.dat*'
	ph = '_w?_*.dat'
	pi = '_wise_images.*'
	pp = '_wise_profile.*'
	im = '_w?*.fit*'
	qf = '_qa.txt'
	jp = '_w?.jpg'
	tl = '_wise_rm'
endif
;
readcol,ilst,id,ra,form='a,d'
nd = n_elements(ra)
;
tmp = ilst
ofil = gettok(tmp,'.') + tl
openw,ol,ofil,/get_lun
;
ddir = !GLGA_ROOT + 'data/'
;
;
for i=0,nd-1 do begin
	gal = strtrim(id[i],2)
	dd = ddir + glga_degdir(ra[i])+'/'
	printf,ol,'rm ' + dd + 'aux/'+gal+a2
	printf,ol,'rm ' + dd + 'aux/'+gal+a3
	printf,ol,'rm ' + dd + 'aux/'+gal+a4
	printf,ol,'rm ' + dd + 'photometry/'+gal+ph
	printf,ol,'rm ' + dd + 'plots/'+gal+pi
	printf,ol,'rm ' + dd + 'plots/'+gal+pp
	printf,ol,'rm ' + dd + dty + 'fits/' + gal+im
	printf,ol,'rm ' + dd + dty + 'jpg/' + gal+jp
	if keyword_set(ellipse) then $
		printf,ol,'rm ' + dd + 'aux/'+gal+a1
	if keyword_set(qa) then $
		printf,ol,'rm ' + dd + dty + 'fits/' + gal+qf
	if keyword_set(build) then $
		printf,ol,'rm -rf ' + dd + dty + 'fits/' + gal
endfor
;
free_lun,ol
return
end
