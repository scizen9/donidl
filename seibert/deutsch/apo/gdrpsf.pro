pro gdrpsf,img,h,x,y,mode=mode,radius=radius

  if (n_elements(mode) eq 0) then mode=0
  if (n_elements(radius) eq 0) then radius=15

  bscentrd,img,x,y,xc,yc

  if (mode eq 0) then begin
    radprof,img,xc,yc,radius
    endif

  gain=3.7

  if (mode eq 0) then begin
    apers=indgen(5)+2
    aper,img,xc,yc,mags,merr,skyv,serr,gain,apers,[15,25], $
      [-32765,32767],/silent

    print,'Sky and err: ',skyv,serr
    bkgns=stdev(img(x+15:x+25,y-5:y+5))
    print,' '
    print,'Background stdev: ',bkgns
    cnts=10^((mags-25)/(-2.5))
    area=!pi*apers^2.0
    poins=10^((mags-25)/(-2.5)) - 10^((mags+merr-25)/(-2.5))
    poins=sqrt(cnts*gain)/gain
    noise=sqrt(area*bkgns^2)
    sn=cnts / sqrt(noise^2 + poins^2)

    print,'Apers:',apers*1.0
    print,'Areas:',area
    print,'Cnts: ',cnts
    print,'PoisE: ',poins
    print,'BkgEr: ',noise
    print,'S/N:   ',sn

    endif






  return
end
