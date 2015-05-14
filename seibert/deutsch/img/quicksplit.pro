pro quicksplit,image1,image2,mindiff=mindiff,ignorelev=ignorelev
;+
; QuickSplit
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

  s=size(image1)
  for i=0,s(2)-1 do begin
    img1=image1(*,i)
    img2=image2(*,i)
    dif=img1-img2

    CR1s=where((dif gt mindiff) and (img2 lt ignorelev))
    CR2s=where((dif lt -1*mindiff) and (img1 lt ignorelev))

    if (CR1s(0) ne -1) then img1(CR1s)=img2(CR1s)
    if (CR2s(0) ne -1) then img2(CR2s)=img1(CR2s)

    image1(*,i)=img1
    image2(*,i)=img2

    if (i/200 eq i/200.) then print,strn(i),' Lines processed...'
    endfor

  print,strn(i),' Lines processed...'
  return

end
