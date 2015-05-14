pro ujchart,filename
;+
;
; UJCHART - an example reading/plotting program for use with the UJ 1.0
;  astrometric catalog.  -Deutsch
;
;-

  if (n_params(0) ne 1) then begin
    print,'Call> ujchart,filename'
    print,"e.g.> ujchart,'test1.out'"
    return
    endif

  openr,1,filename
  lin='' & boxwid=1000.0
  while (strmid(lin,0,8) ne '    Star') do begin
    readf,1,lin
    if (strmid(lin,0,4) eq ' Box') then begin
      boxwid=float(strmid(lin,19,20))
      bw=lin
      endif
    if (strmid(lin,0,4) eq ' Fie') then fc=lin
    endwhile

  nstars=0
  ra=dblarr(1000) & dec=ra & jmag=fltarr(1000) & qual=intarr(1000)
  tstarid='' & dup=''

  while not EOF(1) do begin
    readf,1,tstarid,rah,ram,ras,decd,decm,decs,tjmag,tfield,tqual,dup, $
      format='(a14,2x,i2,i3,f7.3,2x,i3,i3,f6.2,2x,f5.2,2x,i5,2x,i3,2x,a3)'
    if (strn(dup) ne 'D') then begin
      ra(nstars)=rah*1.0d + ram/60.0d + ras/3600.0d
      dec(nstars)=decd*1.0d + decm/60.0d + decs/3600.0d
      jmag(nstars)=tjmag
      qual(nstars)=tqual
      nstars=nstars+1
      endif
    endwhile

  close,1

  ra=ra(0:nstars-1)
  dec=dec(0:nstars-1)
  jmag=jmag(0:nstars-1)
  qual=qual(0:nstars-1)

  ramin=max(ra)+boxwid/3600/15/20
  ramax=min(ra)-boxwid/3600/15/20
  decmin=min(dec)-boxwid/3600/20
  decmax=max(dec)+boxwid/3600/20
  magmin=max(jmag)
  magmax=min(jmag)
  magrng=(magmin-magmax)


  plot,ra,dec,/nodata,xr=[ramin,ramax],yr=[decmin,decmax],xsty=1,ysty=1, $
    ytitle='DEC (J2000)',xtitle='RA (J2000)',title='UJ1.0 Search'

  for i=0,nstars-1 do $
    plots,[ra(i)],[dec(i)],psym=([6,4,4,1,1,1,7,7,7,7])(9-qual(i)), $
      symsize=(magmin-jmag(i))/magrng+.5

  stringad,strmid(fc,26,12)+strmid(fc,43,13),rac,decc
  plots,[rac]/15,[decc],psym=1,symsize=4

  xyouts,0,1.13,/norm,fc
  xyouts,0,1.1,/norm,bw

  return

end



