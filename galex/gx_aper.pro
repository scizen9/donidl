pro gx_aper,fspec,ra,dec,output=output,verbose=verbose
;+
;	gx_aper - perform aperture photometry on a set of images
;
; INPUTS:
;	fspec - a files specification for a group of image files
;	ra,dec - coords of an object in decimal degrees
;-
flist=file_search(fspec,count=nf)
if nf le 0 then begin
	print,'GX_APER: Error - no files found: ',fspec
	return
endif
;
if keyword_set(verbose) then print,'Found this many files: ',nf
;
if keyword_set(output) then $
	ofile = output $
else	ofile = 'gx_aper.out'
openw,ol,ofile,/get_lun
printf,ol,'# GX_APER - '+systime(0)
printf,ol,'# Unix(s),MAG,MAGERR,IMX,IMY,SKY,SKYERR'
;
for i=0,nf-1 do begin
	im=mrdfits(flist[i],0,hdr,/silent)
	extast,hdr,astr
	ad2xy,ra,dec,astr,x,y
	aper,im,x,y,mg,me,sky,skyerr,1.,4.0,[10.,20.],[-1.,100.],/silent
	printf,ol,sxpar(hdr,'EXPSTART'),mg-4.92,me/16.0,x,y,sky,skyerr, $
		form='(f15.3,6f9.3)'
	if keyword_set(verbose) then $
		print,i+1,'/',nf,sxpar(hdr,'EXPSTART'),mg-4.92,me/16.0, $
			x,y,sky,skyerr,form='(i6,a1,i6,f17.3,6f9.3)'
endfor
;
free_lun,ol
;
return
end
