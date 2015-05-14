pro gdrread,img,h,filename,fixpix=fixpix,bias=bias,dark=dark,orien=orien, $
   flat=flat,disp=disp,macfix=macfix,size=imsize,dscrot=dscrot,disrot=disrot
;+
; NAME:
;	GRDREAD
;
; PURPOSE:
;	Read an image from the APO Guider instrument into IDL variables.
;	By default, 512x512 images are expected (1x1 binning on the 512^2
;	camera, but this can be changed with the size= keyword.
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	gdrread,image,header,filename
;	gdrread,image,header,filename [,/fixpix,bias=,dark=,flat=,/orien,
;		/disp,/macfix,size=]
;
; INPUTS:
;	filename: A string containing the name of the image to be read. 
;		The image must be in the guider software native format
;		(which is just a binary dump).
;
; OPTIONAL INPUT KEYWORDS:
;	fixpix:	If set, bad columns on the old SS 1024^2 chip are fixed.
;
;	bias:	If set, an archival bias frame is subtracted.
;
;	dark:	If set, an archival dark frame is subtracted.
;
;	flat:	If set, an archival flat field is applied.
;
;	disrot:	If set, the field is flipped and rotated so that North is
;		up and East is left when the instang=0 if DIS is main inst.
;
;	dscrot:	If set, the field is flipped and rotated so that North is
;		up and East is left when the instang=0 if DSC is main inst.
;
;	disp:	If set, the image is displayed after it is read.
;
;	macfix: If set, the Macintosh binary header is stripped from the
;		file (this is useful if the file was transferred in
;		"MacBinary" mode, although this should be avoided).
;
;	size:	If the image is not 512x512 (the default 1x1 bin of the
;		512^2 chip), then you must specify the size of the image
;		with this keyword.
;
; OUTPUTS:
;	image:	variable into which 2D Guider image array will be stored.
;
;	header:	variable into which Guider image header will be stored.
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
;	Guider image is read into memory, and then any appropriate steps
;	specified by the keywords are performed.
;
; EXAMPLE:
;	window,colors=50
;	whitesky,res='RED'
;	gdrread,img,h,'ewd01',size=512,/disp
;	imgroam,img,h
;
; MODIFICATION HISTORY:
;	05/14/95 Written by Eric W. Deutsch
;	12/18/95 Updated to have 512^2 camera as default adn tidied a bit. EWD
;
;-

  if (n_params(0) ne 3) then begin
    print,'Call> gdrread,img,h,filename,[/bias,/dark,/flat,/fixpix,/orien,/disp,/macfix,size=]'
    print,"e.g.> gdrread,img,h,'g001',/bias,/dark,/fixpix,/orien,/disp"
    return
    endif

  if (n_elements(bias) eq 0) then bias=0
  if (n_elements(dark) eq 0) then dark=0
  if (n_elements(flat) eq 0) then flat=0
  if (n_elements(fixpix) eq 0) then fixpix=0
  if (n_elements(orien) eq 0) then orien=0
  if (n_elements(disp) eq 0) then disp=0
  if (n_elements(macfix) eq 0) then macfix=0
  if (n_elements(imsize) eq 0) then imsize=512

  if (not exist(filename)) then begin
    print,"File '"+filename+"' not found."
    return
    endif

  if (macfix eq 1) then macfix=128
  openr,1,filename
  tmp1=assoc(1,intarr(imsize,imsize),macfix)
  img=tmp1(0)*1.0
  close,1

  tmp2=where(img lt 0)
  if (tmp2(0) ne -1 ) then img(tmp2)=65536.0+img(tmp2)

  mkhdr,h,img

  if (bias eq 1) then begin
    if (imsize eq 341) then begin
      imgread,bias,th,'/host/dione/u5/deutsch/guider/SS1024/cal951010/Zero'
      img=img-bias
      endif
    if (imsize eq 512) then begin
      imgread,bias,th,'/host/dione/u5/deutsch/guider/SS512/cal951217/Zero'
      img=img-bias
      endif
    endif

  if (dark gt 0) then begin
    if (imsize eq 341) then begin
      imgread,darkim,th,'/host/dione/u5/deutsch/guider/SS1024/cal951010/Dark'
      img=img-darkim/60.0*dark
      endif
    if (imsize eq 512) then begin
      imgread,darkim,th,'/host/dione/u5/deutsch/guider/SS512/cal951217/Dark'
      img=img-darkim/60.0*dark
      endif
    endif

  if (flat eq 1) then begin
    if (imsize eq 341) then begin
      imgread,flat,fh,'/host/dione/u5/deutsch/guider/calims/FlatR'
      flat=flat/median(flat)>0.01
      img=img/flat
      endif
    endif

  if (fixpix eq 1) then begin
    if (imsize eq 341) then begin
      img(253,*)=img(252,*)
      img(254,*)=img(255,*)
      endif
    endif

  if (disp gt 0) then begin
    skyline,extrac(img,100-50,240-50,100,100),skyv,rmsv
    v1=skyv-rmsv*1 & v2=skyv+rmsv*10
    if (disp eq 2) then begin
      v1=skyv-rmsv*1 & v2=skyv+rmsv*50
      endif
    endif

  if (orien eq 1) then begin
    img=rotate(reverse(img,1),3)
    endif

  if (n_elements(dscrot) ne 0) then begin
    rot1=fix(dscrot/90)
    img=rotate(reverse(img,2),rot1)
    endif

  if (n_elements(disrot) ne 0) then begin
    rot1=fix(disrot/90)
;    img=rotate(reverse(img,2),rot1+2)		; SS1024
    img=rotate(img,rot1+3)			; SS512
    endif

  if (disp gt 0) then begin
    tv,bytscl(img,v1,v2,top=!d.n_colors-2)
    print,'Displaying min,max: ',v1,v2
    endif

  return
end
