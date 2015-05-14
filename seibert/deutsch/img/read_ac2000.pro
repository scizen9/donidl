pro read_ac2000,filename,ss

  openr,1,filename
  lin='' & i=0

  while not EOF(1) do begin
    readf,1,lin
    stringad,strmid(lin,0,25),ra,dec
    ss.ra(i)=ra & ss.dec(i)=dec
    ss.mag(i)=float(strmid(lin,26,5))
    ss.id(i)=i
    ss.name='AC2000-'+strn(i)
    i=i+1
    endwhile

  ss.stars=i

  close,1
  return

end


