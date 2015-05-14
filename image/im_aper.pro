pro im_aper,fspec,ra,dec,output=output,verbose=verbose,imx=imx,imy=imy, $
	zpoint=zpoint,zpkey=zpkey
;+
;	im_aper - perform aperture photometry on a set of images
;
; INPUTS:
;	fspec - a files specification for a group of image files
;	ra,dec - coords of an object in decimal degrees
;
; KEYWORDS:
;	output - filename for output, if /output then 'im_aper.out'
;	verbose - set to print results
;	imx,imy - set to use image coords instead of WCS and ra,dec
;	zpoint - set to zeropoint magnitude
;	zpkey - set to the zeropoint header keyword
;-
flist=file_search(fspec,count=nf)
if nf le 0 then begin
	print,'IM_APER: Error - no files found: ',fspec
	return
endif
;
if keyword_set(verbose) then print,'Found this many files: ',nf
;
if keyword_set(output) then $
	ofile = output $
else	ofile = 'im_aper.out'
openw,ol,ofile,/get_lun
printf,ol,'# IM_APER - '+systime(0)
printf,ol,'# MAG,MAGERR,IMX,IMY,SKY,SKYERR'
;
for i=0,nf-1 do begin
	im=mrdfits(flist[i],0,hdr,/silent)
	extast,hdr,astr
	ad2xy,ra,dec,astr,x,y
	if keyword_set(zpoint) then $
		zp = zpoint $
	else	if keyword_set(zpkey) then $
		zp = sxpar(hdr,zpkey) $
	else	zp = 25.0
	zpoff = zp - 25.0
	aper,im,x,y,mg,me,sky,skyerr,1.,30.0,[35.,60.],[-100.,32767.];,/silent
	printf,ol,mg+zpoff,me,x,y,sky,skyerr,form='(6f9.3)'
	if keyword_set(verbose) then $
		print,i+1,'/',nf,mg+zpoff,me,x,y,sky,skyerr, $
		form='(i6,a1,i6,2x,6f9.3)'
endfor
;
free_lun,ol
;
return
end
