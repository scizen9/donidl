pro gsca10comp,gscfile,a10file,plot=plot
;+
; NAME:
;	GSCA10COMP
;
; PURPOSE:
;	Compare coordinates of stars in a GSC (gsclist) file and a USNO-A1.0
;	(a10list) file.
;
; CATEGORY:
;	APO software
;
; CALLING SEQUENCE:
;	gsca10comp,gscfile,a10file,[/plot]
;
; INPUTS:
;	gscfile: The filename of an HST Guide Star Catalog (GSC) extraction
;		which contains the target field and the surrounding region.
;
;	a10file: The filename of an USNO-A1.0 extraction
;		which contains the target field and the surrounding region.
;
; OUTPUTS:
;	Screen image or postscript file.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	01/01/97 Written by E. Deutsch
;
;-


; -- Not enough parameters?  Show the call sequence -------------------
  if (n_params(0) lt 2) then begin
    print,'Call> gsca10comp,gscfile,a10file,[/plot]'
    print,"e.g.> gsca10comp,'KA2.gsclist','KA2.a10list',/plot"
    return
    endif

  if (n_elements(plot) eq 0) then plot=''

  if (not exist(gscfile)) then begin
    print,'ERROR: '+gscfile+' not found.'
    return
    endif
  gsc_read,gscfile,ss,/verbose


  if (not exist(a10file)) then begin
    print,'ERROR: '+a10file+' not found.'
    return
    endif
  openr,1,a10file
  lin='' & readf,1,lin & readf,1,lin
  d1=dblarr(8)
  d2=dblarr(8,5000) & i=0
  while not EOF(1) do begin
    readf,1,d1
    d2(*,i)=d1
    i=i+1
    endwhile
  close,1
  d2=d2(*,0:i-1)


  matchrad=6.0
  matches=intarr(n_elements(ss))-1
  for i=0,n_elements(ss)-1 do begin
    dist=sqrt((ss(i).dec-d2(1,*))^2+ $
      ((ss(i).ra-d2(0,*))*cos(ss(i).dec/!radeg))^2)*3600
    close=where(dist lt matchrad)
    if (close(0) ne -1) then begin
      best=close(0)
      if (n_elements(close) gt 1) then begin
        print,'More than one star within ',strn(matchrad), $
          ' arcsec: ',dist(close)
        srt=sort(dist(close))
        best=close(srt(0))
        endif
      matches(i)=best
      print,i,dist(best),d2([3,5],best),ss(i).mag, $
        format='(i5,f7.2,f7.2,i3,f7.2)'
    endif else begin
      print,i,'   No matches within ',strn(matchrad),' arcseconds'
      endelse

    endfor

  good=where(matches ne -1)
  ra0=min(ss(good).ra)
  dec0=min(ss(good).dec)
  d3=dblarr(6,n_elements(good))
  for i=0,n_elements(good)-1 do begin
    x=(ss(good(i)).ra-ra0)*cos(dec0/!radeg)*60
    y=(ss(good(i)).dec-dec0)*60
    dx=(ss(good(i)).ra-d2(0,matches(good(i))))*cos(dec0/!radeg)*3600
    dy=(ss(good(i)).dec-d2(1,matches(good(i))))*3600
    mag=ss(good(i)).mag
    dmag=ss(good(i)).mag - $
      (0.7*d2(2,matches(good(i)))+0.3*d2(3,matches(good(i))))
    d3(*,i)=[x,y,dx,dy,mag,dmag]
    endfor

  print,'avg,median,stdev dX=',avg(d3(2,*)),median(d3(2,*)),stdev(d3(2,*))
  print,'avg,median,stdev dY=',avg(d3(3,*)),median(d3(3,*)),stdev(d3(3,*))

  if (plot ne 0) then begin
    plot,d3(0,*),d3(1,*),psym=4,/nodata
    fac=3.0
    for i=0,n_elements(d3(1,*))-1 do $
      arrow,d3(0,i),d3(1,i),d3(0,i)+d3(2,i)*fac,d3(1,i)+d3(3,i)*fac,/data
    print,'Press [SPACE]...' & key1=get_kbrd(1)
    plot,d3(4,*),d3(5,*),psym=4
    skyline,d3(5,*),av1,rms1
    print,'avg=',av1,'   clipped stdev=',rms1
    endif


  return


end



