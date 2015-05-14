function splfit,x,y,nodes,xs,ys
;+
;			splfit
;
; driver routine for splinefit to compute least squares spline
; with equally spaced nodes.
;
; CALLING SEQUENCE:
;	fit = splfit(x,y,nodes,xs,ys)
;
; INPUTS:
;	x - x data vector
;	y - y data vector
;	nodes - number of spline nodes
;
; OUTPUT:
;	the least square spline, evaluated at points x, is returned
;	as the function value.
;
; OPTIONAL OUTPUTS:
;	xs - x positions of the least squares spline
;	ys - y positions of the least sqaures spline
;
; HISTORY:
;	version 1  D. Lindler   May 1989
;	version 2  DJL/ACC	1/22/92		Moved to IDL Version 2
;	14-JUL-95 Deutsch renamed 'sfit' to 'splfit' since IDL now includes
;		a routine called sfit (surface fitting)
;-
;---------------------------------------------------------------------------
if n_params(0) lt 3 then nodes=10
IF NODES GT 0 THEN BEGIN
	xs=findgen(nodes)*(max(x)-min(x))/(nodes-1)+min(x)
	ys=total(y)/n_elements(y)+xs*0+1
END
w=x*0+1
return,splinefit(x,y,w,xs,ys,SIG,0.01)
end
