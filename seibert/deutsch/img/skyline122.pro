pro skyline122,origline,skyv,rms,interactive

  if (n_params(0) lt 4) then interactive=0

  line=origline(where(origline ne 0))
  els=n_elements(line)
  av1=avg(line)
  rms1=stdev(line)
  if (interactive eq 1) then begin
    print,'1st Iteration: AVG=',strn(av1),',RMS=',strn(rms1)
    plot,indgen(els),line
    oplot,[0,els],[av1,av1]
    oplot,[0,els],[av1+rms1,av1+rms1]
    ch=get_kbrd(1)
    endif

  atmp=where(line lt av1+3*rms1)
  chk=size(atmp)
  if (chk(0) eq 0) then begin
    a=line
    goto,SKIP1
    endif
  a=line(atmp)
  av1=avg(a)
  rms1=stdev(a)
  if (interactive eq 1) then begin
    print,'2nd Iteration: AVG=',strn(av1),',RMS=',strn(rms1)
    plot,indgen(els),a
    oplot,[0,els],[av1,av1]
    oplot,[0,els],[av1+rms1,av1+rms1]
    ch=get_kbrd(1)
    endif

SKIP1:
  atmp=where((a lt av1+2*rms1) and (a gt av1-2*rms1))
  chk=size(atmp)
  if (chk(0) eq 0) then goto,DONE
  a=a(atmp)
  av1=avg(a)
  rms1=stdev(a)
  if (interactive eq 1) then begin
    print,'3nd Iteration: AVG=',strn(av1),',RMS=',strn(rms1)
    plot,indgen(els),a
    oplot,[0,els],[av1,av1]
    oplot,[0,els],[av1+rms1,av1+rms1]
    ch=get_kbrd(1)
    endif

DONE:
  skyv=1.0
;  print,'Skyvalue=',strn(av1),', RMS=',strn(rms1)
  skyv=av1 & rms=rms1

  return
end
