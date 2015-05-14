
ps_start,filename='gmagnlc.ps',/color
setplotcolors

x=findgen(100)*.1+10
y=gmagnlc(x,/nuv)
plot, x,x-y,/ynozero,xr=[20,10],yr=[-.1,3],$
 xtit='Measured Mag',ytit='Non Linear Correction (Mag)',line=3,/nodata
oplot, x,x-y,linestyle=3,color=!red
oplot,[20,10],[0,0]
y=gmagnlc(x,/fuv)
oplot, x,x-y,linestyle=2,color=!blue

ps_end


ps_start,filename='gcountsnlc.ps',/color
setplotcolors

 c0=-1.220
 c1=1.986
 c2=-0.204
 zp=20.08

x=findgen(10000)+1
plot,x,10.^(c0+c1*alog10(x)+c2*(alog10(x))^2),/xlog,/ylog,$
 xtit='predicted',ytit='measured',yr=[1,1e4],/ys,line=3,/nodata
oplot,x,10.^(c0+c1*alog10(x)+c2*(alog10(x))^2),linestyle=3,color=!blue
oplot,[0.1,1e4],[0.1,1e4]

 c0=-0.297
 c1=1.468
 c2=-0.174
 zp=18.82

oplot,x,10.^(c0+c1*alog10(x)+c2*(alog10(x))^2),linestyle=2,color=!red

ps_end

end
