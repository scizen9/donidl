pro ir_radprof,img,xc,yc

  radius=6 & expand=2
  radprof,img,xc,yc,radius,expand=expand

  w1=widget_base(/column,title=' ',xoffset=700,yoffset=400)
  w10=widget_base(w1,/row)
  w11=widget_label(w10,value='Radius [2-50] ')
  w12=widget_text(w10,/editable,xsize=20,ysize=1,value='6')
  w20=widget_base(w1,/row)
  w21=widget_label(w20,value='Expansion [1-6] ')
  w22=widget_text(w20,/editable,xsize=20,ysize=1,value='2')
  w2=widget_button(w1,value='          DONE     ') 

  widget_control,w1,/realize
  Ret_Val=-1
  while (Ret_Val(0) eq -1) do begin
    event=WIDGET_EVENT(w1)

    if (event.ID eq w12) then begin
      widget_control,w12,get_value=newradius
      newradius=newradius(0)
      if (strnumber(newradius) eq 1) then begin
	radius=fix(newradius)
	if (radius lt 2) or (radius gt 50) then begin
	  radius=6
	  widget_control,w11,set_value='  Out of Range'
	  wait,1 & widget_control,w12,set_value='6'
	endif else radprof,img,xc,yc,radius,expand=expand
      endif else begin
	widget_control,w11,set_value='  Invalid'
	wait,1 & widget_control,w12,set_value='6'
	endelse
      endif

    if (event.ID eq w22) then begin
      widget_control,w22,get_value=newexpand
      newexpand=newexpand(0)
      if (strnumber(newexpand) eq 1) then begin
	expand=fix(newexpand)
	if (expand lt 1) or (expand gt 6) then begin
	  expand=2
	  widget_control,w21,set_value='  Out of Range'
	  wait,1 & widget_control,w12,set_value='2'
	endif else radprof,img,xc,yc,radius,expand=expand
      endif else begin
	widget_control,w21,set_value='  Invalid'
	wait,1 & widget_control,w22,set_value='2'
	endelse
      endif

    if (event.ID eq w2) then Ret_Val=1

    endwhile

  widget_control,w1,/destroy
  return

end
