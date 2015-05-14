pro gdrflatexam,size=imsize
;+
; NAME:
;	GDRFLATEXAM
;
; PURPOSE:
;	Examine the files FLAT[1-9].
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	gdrflatexam
;	gdrflatexam,[size=]
;
; INPUTS:
;	none
;
; OPTIONAL INPUT KEYWORDS:
;	size:	If the images are not 512x512 (the default 1x1 bin of the
;		512^2 chip), then you must specify the size of the image
;		with this keyword.
;
; OUTPUTS:
;	None
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	
;
; EXAMPLE:
;	gdrmkcals
;
; MODIFICATION HISTORY:
;	12/18/95 Written by E. Deutsch
;
;-

  if (n_params(0) ne 0) then begin
    print,'Call> gdrflatexam,size=]'
    print,"e.g.> gdrflatexam"
    return
    endif

  if (n_elements(imsize) eq 0) then imsize=512

  imgread,zero,zh,'Zero'
  imgread,dark,zh,'Dark'

  cube=fltarr(imsize,imsize,9)
  exposure=[1,2,4,6,8,10,12,14,14]*1.0

  for i=0,8 do begin
    filename='FLAT'+strn(i+1)
    if (not exist(filename)) then begin
      print,"File '"+filename+"' not found.  Aborting calibration sequence."
      return
      endif
    print,'Reading '+filename+'...'
    gdrread,img,h,filename,size=imsize
    cube(*,*,i)=img-zero-dark*(exposure(i)/60)
    endfor

  flat=smooth(cube(*,*,7)+cube(*,*,8),5)
  flat=flat/avg(flat)

  data=fltarr(2,9*16)

  for i=0,8 do begin
    tmp1=cube(*,*,i)
    tmp1f=cube(*,*,i)/flat
    for j=0,15 do begin
      y=j/4 & x=j-j/4*4
      x=x*(imsize/5.0)+50 & y=y*(imsize/5.0)+50
      tmp2=tmp1(x:x+30,y:y+30)
      tmp2f=tmp1f(x:x+30,y:y+30)
      data(0,i*16+j)=avg(tmp2)
      data(1,i*16+j)=stdev(tmp2f)^2
      endfor
    endfor

  plot,data(0,*),data(1,*),psym=4

stop




  return

end
