pro gx_vs_get_data,cfile
;+
; gx_vs_get_data - read in a csv file output from visit server and
; 		copy over the image files
;-
readcol,cfile,j1,j2,j3,j4,j5,j6,j7,j8,j9,j10,j11,j12,im,j14,j15,dd, $
	form='i,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a'
nf = n_elements(im)
;
fdecomp,cfile,disk,dir,rute
ofile=rute+'_cp'
filestamp,ofile
openw,ol,ofile,/get_lun
;
; loop over images
for i=0,nf-1 do begin
	print,im[i]
	printf,ol,'cp '+dd[i]+'/'+im[i]+'-?d-cnt.fit* .'
	printf,ol,'cp '+dd[i]+'/'+im[i]+'-?d-rrhr.fit* .'
endfor
;
free_lun,ol
;
return
end
