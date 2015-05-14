pro disslit,img,xguess,yguess,width,yfit=yfit
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) lt 2) then begin
    print,"Call> disslit,image,xguess,yguess,[width,yfit=]"
    print,""
    print,"e.g.> disread,img,h,'/host/bluemoon/scr/tmp/deutsch/n3/n3.0002r'
    print,"e.g.> disslit,img,343,245"
    print,""
    return
    endif


  if (n_elements(width) eq 0) then width=10

  npts=n_elements(xguess)
  yfit=fltarr(npts)

  for ii=0,npts-1 do begin

    xi=fix(xguess(ii)) & yi=fix(yguess)
    band=fltarr(30)
    for i=0,width-1 do band=band+img(xi-fix(width/2.0+.5)+i,yi-15:yi+14)
    band=band/width

    x=findgen(30)
    plot,x+yi-15,band,psym=4,xtitle='Y coordinate'

    fit=gaussfit(x,band,coeff)

    oplot,x+yi-15,fit
    oplot,[coeff(1)+yi-15,coeff(1)+yi-15],[-1000,65000.0],linesty=1

    print,'Slit height at X=',strn(xi),' is Y=',strn(coeff(1)+yi-15)
    yfit(ii)=coeff(1)+yi-15

    xyouts,.15,.91,/norm,'Slit height at X='+strn(xi)+ $
      ' is Y='+strn(coeff(1)+yi-15),charsize=1.3

    if (npts eq 999) then begin
      xyouts,.15,.85,/norm,'Click mouse to continue....',charsize=1.3
      cursor,x1,y1,/wait
      endif

    endfor

;  wdelete,7


  return

end
