pro GS_Disp,ss,NoList=NoList

; This procedure displays information in the astrometry package
; in the IMGroam environment.  It is not useful by itself.

  COMMON ANSI,cr,lf,ESC,up,clrscrn,bell
  COMMON GSGET_PARAMS,GSfile,target_ra,target_dec,FOV,stars,star_labeling
  COMMON PLPAR,xmn,ymn,xmx,ymx,GuideStWIN,ssptr

  lines=0 & i=0
  print,clrscrn,'   ##### Guide Star Selection Parameters #####'
  print,'Current Guide Star File: ',GSfile
  if (GSfile ne 'NONE') then begin
    FOV=abs(ymn-ymx)
    print,'Coordinates of Target: ',vect([target_ra,target_dec]),'    (degrees)'
    print,'Field of View: ',strn(FOV*60),' (arcmin)   Star labeling: ',YesNo2(star_labeling/2,type=6)
    lines=lines+2
    endif
  print,'Stars Loaded: ',strn(stars),'      Stars Selected: ',strn(ss.stars)

  print,cr,'  #   ID #     RA        DEC       MAG        X       Y'
  lines=lines+2
  if (ss.stars gt 0) and (n_elements(NoList) eq 0) then begin
    fmt='$(i3,i7,2x,f8.4,2x,f8.4,4x,f5.2,3x,2f8.2)'
    for i=0,ss.stars-1 do begin
      print,fmt,i,ss.id(i),ss.ra(i),ss.dec(i),ss.mag(i),ss.x(i),ss.y(i)
      ss.id(i)=i
      if (i eq ssptr) then print,up,'->'
      lines=lines+1
      endfor
    endif
  if (i eq ssptr) then print,'->',up

  while (lines lt 15) do begin 
    print,''
    lines=lines+1
    endwhile

  return
end
