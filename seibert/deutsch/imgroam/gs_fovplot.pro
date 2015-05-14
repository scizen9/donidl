pro GS_FOVplot,ls,ss,flag,onestar

; This procedure displays a loaded HST Guide star catalog
; in the IMGroam environment.  It is not useful by itself.

  COMMON PLPAR,xmn,ymn,xmx,ymx,GuideStWIN
  COMMON GSGET_PARAMS,GSfile,target_ra,target_dec,FOV,stars,star_labeling

  !xtitle='RA (degrees)'
  !ytitle='DEC (degrees)'
  !mtitle='Guide Star Field'
  if (!d.name eq 'X') then win_set,GuideStWIN

  if ((flag and 1) eq 1) then begin
    xmn=max(ls.ra) & xmx=min(ls.ra)
    ymn=min(ls.dec) & ymx=max(ls.dec)
    endif

  rang=max(ls.mag)-min(ls.mag)
  maxval=max(ls.mag)
  factr=128./rang

  usersym,[0,1,1,0,0],[0,0,1,1,0],/fill
  xrang=xmx-xmn & yrang=ymx-ymn

  if ((flag and 2) eq 2) then begin
    i=onestar
    oplot,[ss.ra(i),ss.ra(i)],[ss.dec(i),ss.dec(i)],psym=8, $
      symsize=-(ss.mag(i)-maxval)*factr/96.+.4,color=128
    return
    endif

  plot,[0,0],[0,0],xrange=[xmn,xmx],yrange=[ymn,ymx],xstyle=1,ystyle=1, $
    /NODATA,charsize=1.5
  oplot,[target_ra,target_ra],[target_dec,target_dec],psym=2,symsize=2

  for i=0,n_elements(ls)-1 do begin
    oplot,[ls(i).ra],[ls(i).dec],psym=8, $
      symsize=-(ls(i).mag-maxval)*factr/96.+.4,color=255
    if (star_labeling eq 1) then begin
      xyouts,ls(i).ra+xrang/90.,ls(i).dec+yrang/90.,strn(ls(i).id),noclip=0
      endif
    endfor

  for i=0,ss.stars-1 do begin
    oplot,[ss.ra(i),ss.ra(i)],[ss.dec(i),ss.dec(i)],psym=8, $
      symsize=-(ss.mag(i)-maxval)*factr/96.+.4,color=128
    endfor

  xrang=xmx-xmn & yrang=ymx-ymn
  xcent=(xmn+xrang/2.) & ycent=(ymn+yrang/2.)
  x0=xcent-xrang/2.-xrang/15.
  y0=ycent-yrang/2.-yrang/10.
  xoff=abs(xrang)/10.
  yoff=abs(yrang)/10.
  oplot,[x0,x0],[y0,y0+yoff],color=255,noclip=1,thick=2
  oplot,[x0,x0+xoff],[y0,y0],color=255,noclip=1,thick=2
  xyouts,x0+xrang/50.,y0+yoff*1.1,'N'
  xyouts,x0+xoff*1.3,y0-yrang/50.,'E'

  return
end
