pro quicksplit2,image1,image2,mindiff=mindiff,ignorelev=ignorelev
;+
; QuickSplit2
;
; This program is supposed to be a real quick and dirty CR-split program
;
; IDL> quicksplit,image1,image2,[mindiff=,ignorelev=]
;
; image1 and image2 are the two images to be split
; mindiff= is a keyword for the minimum difference considered to be a CR
; ignorelev= is a keyword which specifies the level above which CR's are
;      not to be checked for. (like stars)
; default values are 20 and 100 respectively
;
;-


  if (n_elements(mindiff) eq 0) then mindiff=20.
  if (n_elements(ignorelev) eq 0) then ignorelev=100.

  gain=8.0
  nCR1s=0.0 & nCR2s=0.0
  s=size(image1)
  for i=0,s(2)-1 do begin
    img1=image1(*,i)
    img2=image2(*,i)
    lower=img1<img2
    sig=sqrt(13.0^2 + lower*gain)/gain

    CR1s=where(img1-lower gt sig*6)
    CR2s=where(img2-lower gt sig*6)

    if (CR1s(0) ne -1) then begin
      img1(CR1s)=img2(CR1s)
      nCR1s=nCR1s+n_elements(CR1s)
      endif
    if (CR2s(0) ne -1) then begin
      img2(CR2s)=img1(CR2s)
      nCR2s=nCR2s+n_elements(CR1s)
      endif

    image1(*,i)=img1
    image2(*,i)=img2

    if (i/200 eq i/200.) then print,strn(i),' Lines processed...'
    endfor

  print,strn(i),' Lines processed...'
  print,strn(nCR1s),' cosmic ray pixels fixed in image 1'
  print,strn(nCR2s),' cosmic ray pixels fixed in image 2'

  return

end
