; $Id: pan_gaussian.pro,v 1.1 2010/05/14 16:32:58 neill Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function pan_gaussian,x,parms,$
                    	parmnames = parmnames, $
                    	canDraw = canDraw, $
                    	drawMessage = drawMessage, $
                    	_Extra = extra

if n_params() eq 0 then begin
  parmnames = ['area','center','FWHM']
  return,-1
endif
drawMessage = strarr(4)
drawMessage[0:1] = ['Hold left mouse button down','and drag function centroid']
drawMessage[2:3] = ['Hold left mouse button down','and drag to change width']
fwhm = parms[2]
sig = fwhm/sqrt(8.0*alog(2.0))
area = parms[0]
cen = parms[1]
yout = (area/sqrt(2.0*!dpi*sig^2))*exp(-0.5*((x-cen)/sig)^2)
;yout = (peak)*exp(-((x-cen)^2 * 4*alog(2))/fwhm^2)
canDraw = 1
return,yout
end
