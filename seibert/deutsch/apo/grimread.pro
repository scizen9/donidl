pro grimread,img,h,filenamein,noproc=noproc,noflat=noflat,levfit=levfit,$
      darkfile=darkfile,flatfile=flatfile,flagdefects=flagdefects,disp=disp, $
      norot=norot,fixdefects=fixdefects,silent=silent
;+
; NAME:
;	GRIMREAD
;
; PURPOSE:
;	Read an image from the APO GRIM II instrument into IDL variables.
;	By default, the image is dark-subtracted, flat-fielded, and flipped
;	and rotated so that North is up and East is left when the instrument
;	angle is 0.
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	grimread,image,header,filename
;	grimread,image,header,filename [,/noproc,/noflat,/levfit,/flagdefects,
;		darkfile=,flatfile=,/disp,/norot]
;
; INPUTS:
;	filename: A string containing the name of the image to be read. 
;		The image may be a GEIS file (.hhh & .hhd), an OIF file
;		(.imh & .pix) or a FITS file (.fit or .fits).  If no
;		no file extention is specified, .hhh is assumed.
;
; OPTIONAL INPUT KEYWORDS:
;	noproc:	If set, no processing or rotating is done to the image.
;
;	noproc:	If set, a flat-field is not applied to the image.
;
;	levfit:	There are often differences in bias levels (and even a
;		curved level) between the four quadrants.  Set this
;		keyword to attempt to fit and remove this level differences.
;
;	flatdefects: Flag the bad pixels as determined by a bad pixel file.
;		All bad pixels are given the value 65535.0
;
;	darkfile: A string containing the filename of a dark file to be
;		subtracted from the image instead of the default dark.
;
;	flatfile: A string containing the filename of a flat-field file to be
;		divided into the image instead of the default flat.
;
;	disp: If set, the image is also displayed.
;
;	norot: If set, the image is is NOT rotated 180 degrees.
;
; OUTPUTS:
;	image:	variable into which 2D GRIM image array will be stored.
;
;	header:	variable into which GRIM image header will be stored.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Approximate image stretching parameters are stored in the header.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	GEIS image is read into memory, and then a dark is subtracted, the
;	image is flat-fielded with archival calibration images.  If desired,
;	bias levels are fit and corrected, and bad pixels are flagged to
;	65535.0.  Finally the image is flipped and rotated, and approximate
;	stretching parameters are stored in the header.
;
; EXAMPLE:
;	window,colors=50
;	whitesky,res='RED'
;	grimread,img,h,'/host/dione/u3/deutsch/grim/sample/n1.0004',/flagdef
;	imgroam,img,h
;
; MODIFICATION HISTORY:
;	1995 Written by E. Deutsch
;
;-


  if (n_params(0) lt 3) then begin
    print,"Call> grimread,image,header,filename,[/noproc,/noflat,/levfit,darkfile=,"
    print,"        flatfile=,/disp,/norot]"
    print,"e.g.> grimread,img,h,'n1.0001',/disp"
    print,"  /noproc      don't dark, flat, etc.  Just orient Nup-Eleft"
    print,"  /noflat      don't flat.  Just orient Nup-Eleft and dark"
    print,"  /levfit      try to correct for the different quadrant levels"
    print,"  /flagdefects flag pixels in default bad pixel map to 65535"
    print,"  darkfile=    string containing the name of a custom dark file"
    print,"  flatfile=    string containing the name of a custom flat file"
    print,"  /disp        display the image, too"
    print,"  /norot       don't rotate the image 180 degrees"
    return
    endif

  if (n_elements(noproc) eq 0) then noproc=0
  if (n_elements(noflat) eq 0) then noflat=0
  if (n_elements(levfit) eq 0) then levfit=0
  if (n_elements(flagdefects) eq 0) then flagdefects=0
  if (n_elements(fixdefects) eq 0) then fixdefects=0
  if (n_elements(darkfile) eq 0) then darkfile=''
  if (n_elements(flatfile) eq 0) then flatfile=''
  if (n_elements(disp) eq 0) then disp=0
  if (n_elements(norot) eq 0) then norot=0
  if (fixdefects eq 1) then flagdefects=1
  if (n_elements(silent) eq 0) then silent=0


; ---------------------------------------------------------------------------
  COMMON GRIM_COMM,dark5,dark10,dark20,kflat,jflat,badpix

; ---------------------------------------------------------------------------
  ffmt='GEIS' & filename=filenamein
  ext=strmid(filename,strlen(filename)-4>0,199)
  if (ext eq '.fit') or (ext eq 'fits') then ffmt='FITS'
  if (ext eq '.imh') then ffmt='OIF'
  if (ffmt eq 'GEIS') and (ext ne '.hhh') then filename=filename+'.hhh'

  if not exist(filename) then begin
    print,'File "',filename,'" not found.'
    return
    endif

  img=0
  if (ffmt eq 'GEIS') then imgread,img,h,filename,silent=silent
  if (ffmt eq 'FITS') then begin
    print,'Reading file "',filename,'"'
    img=readfits(filename,h)
    img=img+10000
    endif
  if (ffmt eq 'OIF') then begin
    print,'Reading file "',filename,'"'
    irafrd,img,h,filename
    endif
  if (n_elements(img) lt 1000) then return

  img1=img
  img=img*1.0
  tmp1=where(img lt 0)
  if (tmp1(0) ne -1 ) then img(tmp1)=65536.0+img(tmp1)

  if (n_elements(badpix) lt 5) then $
    imgread,badpix,dh,'/host/dione/u5/deutsch/apo/grim/calib/badpixmap'

; ---------------------------------------------------------------------------

  if (noproc) then goto,STRETCH

  dark=999.9
  if (darkfile ne '') then begin
    if not exist(darkfile) then begin
      print,'Specified darkfile "',darkfile,'" not found.'
      return
      endif
    imgread,dark,dh,darkfile
    goto,SKIPDRD
    endif

  if (n_elements(dark5) lt 5) then $
    imgread,dark5,dh,'/host/dione/u5/deutsch/apo/grim/calib/Dark5'
  if (n_elements(dark10) lt 5) then $
    imgread,dark10,dh,'/host/dione/u5/deutsch/apo/grim/calib/Dark10'
  if (n_elements(dark20) lt 5) then $
    imgread,dark20,dh,'/host/dione/u5/deutsch/apo/grim/calib/Dark20'

  exptime=sxpar(h,'OPENTIME')
  if (exptime eq 5-1.091) then dark=dark5
  if (exptime eq 10-1.091) then dark=dark10
  if (exptime eq 20-1.091) then dark=dark20
  if (dark(0) eq 999.9) then begin
    if (exptime lt 10-1.091) then begin
      diff=(dark10-dark5)/5
      dark=dark5+diff*(exptime-(5-1.091))
      endif
    if (exptime gt 10) then begin
      diff=(dark20-dark10)/10
      dark=dark10+diff*(exptime-(10-1.091))
      endif
    endif

SKIPDRD:

  print,'Subtracting dark frame - median value: ',strn(median(dark))
  img=img-dark

; ---------------------------------------------------------------------------
; ---------------------------------------------------------------------------

  if (noflat) then goto,STRETCH

  flat=999.9
  if (flatfile ne '') then begin
    if not exist(flatfile) then begin
      print,'Specified flatfile "',flatfile,'" not found.'
      return
      endif
    imgread,flat,dh,flatfile & fn='user-specified'
    flat=flat/median(flat)
    tmp11=where(flat eq 0)
    if (tmp11(0) ne -1) then flat(tmp11)=1
    goto,SKIPFRD
    endif

  if (n_elements(kflat) lt 5) then begin
    imgread,kflat,dh,'/host/dione/u5/deutsch/apo/grim/calib/FlatK'
    kflat=kflat/median(kflat)
    tmp11=where(kflat eq 0)
    if (tmp11(0) ne -1) then kflat(tmp11)=1
    endif
 if (n_elements(jflat) lt 5) then begin
    imgread,jflat,dh,'/host/dione/d3/deutsch/apo/jan12/jflat1/Flat_sky_j20'
    jflat=jflat/median(jflat)
    tmp11=where(jflat eq 0)
    if (tmp11(0) ne -1) then jflat(tmp11)=1
    endif

  filt=sxpar(h,'FILTER1')
  if (filt eq 1) then begin & flat=jflat & fn='J' & endif
  if (filt eq 3) then begin & flat=kflat & fn='K' & endif
  if (filt eq 4) then begin & flat=kflat & fn='K''' & endif
  if (flat(0) eq 999.9) then begin
    print,'Unknown filter -',strn(filt),'-.  Using K flat...'
    flat=kflat & fn='K'
    endif


SKIPFRD:
  print,'Applying ',fn,' flat field...'
  img=img/flat


; ---------------------------------------------------------------------------
; Try to compensate for different amplifier levels
  if (levfit eq 1) then begin
    print,'Compensating for the different quadrant levels..'
    skyv=median(img)
    corr=fltarr(2,256)
    for y=0,1 do begin
      for x=0,1 do begin
        tmp1=fltarr(128)
        for j=0,127 do tmp1(j)=median(img(128*x:128*x+127,j+128*y))
        tmp2=splfit(findgen(128),tmp1,5)
        corr(x,y*128:y*128+127)=tmp2
        endfor
      endfor
    corr=corr-skyv
    for j=0,255 do img(j,*)=img(j,*)-corr(j/128,*)
    endif


STRETCH:
; ------------------------------------------------------------------------
; Restore pixels with DN=65535 in the raw image to 65535 in the processed image
  tmp1=where((img1 eq 65535.0))
  if (tmp1(0) ne -1 ) then img(tmp1)=65535.0

; ------------------------------------------------------------------------
; Set bad pixels to 65535 is flagdefects flag is set
  if (flagdefects eq 1) then begin
    bad=where(badpix ne 0)
    img(bad)=65535.0
    endif
  if (fixdefects eq 1) then begin
    tmpbadpix1=badpix
    tmpbadpix2=badpix
    img2=img
    while (max(tmpbadpix1) gt 0) do begin
      tmpbadpix2=shift(tmpbadpix2,1)
      img2=shift(img2,1)
      bad=where(tmpbadpix1 ne 0)
      tmp1=where(tmpbadpix2(bad) eq 0)
      if (tmp1(0) ne -1) then begin
        repl=bad(tmp1)
        img(repl)=img2(repl)
        tmpbadpix1(repl)=0
        endif
      endwhile
    endif

; ------------------------------------------------------------------------
; Rotate or flip the image so North is up and East is left when instang=0
  if (norot eq 0) then begin
    lens=sxpar(h,'LENS')
    if (lens eq 1) then begin
      print,'Rotating image 180 degrees..'
      img=rotate(img,2)
      endif
    if (lens ge 2) then begin
      print,'Flipping image along Y axis..'
      img=reverse(img,1)
      endif
    endif


; ------------------------------------------------------------------------
; Determine some approximate scaling paramaters
  skyv=median(img)
  rmsv=sqrt(15.0^2 + (sqrt(skyv>1)/1.7)^2)
  scmin=skyv-rmsv*1
  scmax=skyv+rmsv*7


; ------------------------------------------------------------------------
; Update the header with scaling parameters
  check_FITS,img,h,/UPDATE
  sxaddpar,h,'IR_SCMIN',scmin,' IMGRoam Frame Scaling Minimum'
  sxaddpar,h,'IR_SCMAX',scmax,' IMGRoam Frame Scaling Maximum'
  sxaddpar,h,'IR_RDTYP','NONE',' IMGRoam Frame Reduction Type'
  sxaddpar,h,'IR_SATLM',65535.0,' IMGRoam Saturation Limit Value'
  if not silent then print,'Sky Level and rms: ',skyv,rmsv


; ------------------------------------------------------------------------
; Display the image
  if (disp eq 1) and (!d.name eq 'X') then begin
    if (!d.window ne 6) then window,6,xs=512,ys=512
    tp1=(!d.n_colors<256)-1
    lgmax=(scmax-scmin)*8+scmin

    dim=imscl(img-scmin,0,lgmax-scmin,scmax-scmin,top=tp1-1)
    tmp1=where(img eq 65535.0)
    if (tmp1(0) ne -1) then dim(tmp1)=tp1
    tv,congrid(dim,512,512)

    if (norot eq 0) then begin
      arrow,.05,.01,.01,.01,/norm
      arrow,.05,.01,.05,.07,/norm
      endif

    endif



  return
end
