pro gx_background,im,skyval,aprad=aprad,appos=appos,hplot=hplot, $
	mmm=mmm,meanclip=meanclip
;
; analyze image for background
;
;	im - input count image (countrate * expo)
;
; 	aprad - object aperture radius (to be masked)
;	appos - center position of aperture
;	circlerad - radius outside of which to mask
;
; init
skyval=0.
;
; image size
s=size(im)
if s[0] ne 2 then begin
	print,'ERROR - image must be 2 dimensional'
	return
endif
nx = s[1]
ny = s[2]
mskim = intarr(nx,ny)
;
; circlerad
if keyword_set(circlerad) then begin
	dist_circle,dis,[nx,ny]
	mask=where(dis ge circlerad, nmsk)
	if nmsk gt 0 then $
		mskim(mask) = 1
endif
;
; object aperture
if keyword_set(aprad) then begin
	if keyword_set(appos) then begin
		if n_elements(appos) ne 2 then begin
			print,'ERROR - bad ap position (x,y): ',appos
			return
		endif
		x0 = appos(0)
		y0 = appos(1)
	endif else begin
		x0 = float(nx)/2.
		y0 = float(ny)/2.
	endelse
	dist_circle,dis,[nx,ny],x0,y0
	mask=where(dis le aprad, nmsk)
	if nmsk gt 0 then $
		mskim(mask) = 1
endif
;
; get sky data
good=where(mask ne 1, ngood)
if ngood le 6 then begin
	print,'ERROR - not enough good sky pixels: ',ngood
	return
endif
sky=im(good)
;
; calculate with different methods
mmm,sky,skm,sksgm
meanclip,sky,skc,sksgc
ims,sky,ski,sksgi
if keyword_set(mmm) then $
	skyval=skm $
else	skyval=skc
;
; plot histogram
if keyword_set(hplot) then begin
  h=histogram(sky,loc=xh)
  plot,xh,h,psym=10,thick=3,charsi=1.7,charthi=3,title='SKY HISTOGRAM', $
	xtitle='SKY COUNTS', ytitle='N'
  oplot,[skm,skm],[0,1.e6]
  oplot,[skc,skc],[0,1.e6],linesty=1
  oplot,[ski,ski],[0,1.e6],linesty=2
  fmt='(f9.5)'
  legend,[string(skm,form=fmt) + ' +- ' + string(sksgm,form=fmt) + '  MMM', $
	  string(skc,form=fmt) + ' +- ' + string(sksgc,form=fmt) + '  MNC', $
	  string(ski,form=fmt) + ' +- ' + string(sksgi,form=fmt) + '  IMS'], $
	  linesty=[0,1,2],box=0,/right,charsi=1.7,charthi=3
endif
;
return
end
