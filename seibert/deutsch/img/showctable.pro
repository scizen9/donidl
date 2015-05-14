pro showctable,dummy

  plot,[0,110],[0,270],/nodata,xsty=5,ysty=5,pos=[0,0,1,1]

  for y=1,26 do begin
    for x=1,10 do begin
      fac=(y-1)*10+(x-1)
      if (fac lt 256) then $
        xyouts,x*10,y*10,'!17'+strn(fac),charsize=1.6,color=fac
      endfor
    endfor

end

