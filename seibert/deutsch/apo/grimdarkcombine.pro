pro grimdarkcombine,searchspec,outputfile
;+
; NAME:
;	GRIMDARKCOMBINE
;
; PURPOSE:
;	Combine a set of dark frames.
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	grimdarkcombine,searchspec
;	grimlist,searchspec [,outputfilename]
;
; INPUTS:
;	searchspec: A string containing the search specification for the
;		images to be combined.  This is typically '*.hhh'
;
; OPTIONAL INPUTS:
;	outputfilename: A string containing the name of an output file
;		which will contain the final dark.  If this
;		paramenter is not supplied, the name will be 'Dark'
;
; OPTIONAL INPUT KEYWORDS:
;	None.
;
; OUTPUTS:
;	None.
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
;	A list of images is assembled, and then the images are average combined.
;
; EXAMPLE:
;	grimdarkcombine,'darks/*.hhh'
;
; MODIFICATION HISTORY:
;	1995 Written by E. Deutsch
;
;-

  if (n_params(0) lt 1) then begin
    print,"Call> grimdarkcombine,searchspec"
    print,"Call> grimlist,searchspec [,outputfilename]"
    print,"e.g.> grimdarkcombine,'darks/*.hhh'"
    return
    endif

  files=findfile(searchspec)
  nfiles=n_elements(files)
  if (nfiles eq 1) and (files(0) eq '') then begin
    print,'Unable to find any files with search spec: ',searchspec
    return
    endif

  if (n_elements(outputfile) eq 0) then outputfile='Dark'

  window,xs=512
  tp=(!d.n_colors<256)-1

  cube=fltarr(256,256,nfiles)
  dark=fltarr(256,256)

  xpos=0 & ypos=0
  for i=0,nfiles-1 do begin
    grimread,img,h,files(i),/noproc,/norot
    cube(*,*,i)=img
    dark=dark+img
    skyline,img(157:181,51:74),s1,r1 & print,s1,r1
    tv,bytscl(img,s1-r1*5,s1+r1*15,top=tp-1),xpos*256,ypos*256
    xpos=xpos+1
    if (xpos eq 2) then begin
      ypos=ypos+1 & xpos=0
      if (ypos eq 2) then ypos=0
      endif
    endfor

  dark=dark/nfiles

  if (1 eq 1) then begin
    rmsarr=img*0
    for y=0,255 do begin
      for x=0,255 do begin
        pix=cube(x,y,*)
        rmsarr(x,y)=stdev(pix)
        endfor
      endfor
    endif

  tv,congrid(bytscl(rmsarr,0,median(rmsarr)*3,top=tp-1),512,512)

  stwrt,dark,h,outputfile,/sdas
  stwrt,rmsarr,h,outputfile+'_stdev',/sdas

  wait,4


  xpos=0 & ypos=0
  for i=0,nfiles-1 do begin
    grimread,img,h,files(i),/noproc,/norot
    img=img-dark
    skyline,img(157:181,51:74),s1,r1 & print,s1,r1
    tv,bytscl(img,s1-r1*5,s1+r1*15,top=tp-1),xpos*256,ypos*256
    xyouts,xpos*256+10,ypos*256+10,strn(files(i)),/device
    wait,0.5
    xpos=xpos+1
    if (xpos eq 2) then begin
      ypos=ypos+1 & xpos=0
      if (ypos eq 2) then ypos=0
      endif
    endfor


  return

end



