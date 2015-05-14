pro grimbadpixmap,searchspec,outputfile,darkfile=darkfile,flatfile=flatfile
;+
; NAME:
;	GRIMBADPIXMAP
;
; PURPOSE:
;	Combine a set of dark frames.
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	grimbadpixmap,searchspec
;	grimbadpixmap,searchspec,[outputfilename]
;
; INPUTS:
;	searchspec: A string containing the search specification for the
;		images to be used.  This is typically '*.hhh'
;
; OPTIONAL INPUTS:
;	outputfilename: A string containing the name of an output file
;		which will contain the bad pixel map.  If this
;		paramenter is not supplied, the name will be 'badpix'
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
;	grimbadpixmap,'dome/*.hhh'
;
; MODIFICATION HISTORY:
;	1995 Written by E. Deutsch
;
;-

  if (n_params(0) lt 1) then begin
    print,"Call> grimbadpixmap,searchspec"
    print,"Call> grimbadpixmap,searchspec [,outputfilename]"
    print,"e.g.> grimbadpixmap,'dome/*.hhh'"
    return
    endif

  files=findfile(searchspec)
  nfiles=n_elements(files)
  if (nfiles eq 1) and (files(0) eq '') then begin
    print,'Unable to find any files with search spec: ',searchspec
    return
    endif

  if (n_elements(outputfile) eq 0) then outputfile='badpixmap'

  window,xs=512
  tp=(!d.n_colors<256)-1

  cube=fltarr(256,256,nfiles)

  xpos=0 & ypos=0
  for i=0,nfiles-1 do begin
    grimread,img,h,files(i),/norot,dark=darkfile,flat=flatfile,/levfit
    cube(*,*,i)=img
    skyline,img(157:181,51:74),s1,r1 & print,s1,r1
    if (i eq 0) then refsky=s1 else img=img/(refsky/s1)
    skyline,img(157:181,51:74),s1,r1 & print,s1,r1
    tv,bytscl(img,s1-r1*5,s1+r1*15,top=tp-1),xpos*256,ypos*256
    xpos=xpos+1
    if (xpos eq 2) then begin
      ypos=ypos+1 & xpos=0
      if (ypos eq 2) then ypos=0
      endif
    endfor


  rmsarr=img*0
  for y=0,255 do begin
    for x=0,255 do begin
      pix=cube(x,y,*)
      spix=pix(sort(pix))
      rmsarr(x,y)=stdev(spix(0:nfiles-4))
      endfor
    endfor

  tmp1=bytscl(rmsarr,150,300,top=tp-1)
  tmp2=where(rmsarr gt 300)
  tmp1(tmp2)=tp
  tv,congrid(tmp1,512,512)

  badpix=rmsarr
  badpix(where(badpix lt 150))=0

  stwrt,badpix,h,outputfile,/sdas


  return

end



