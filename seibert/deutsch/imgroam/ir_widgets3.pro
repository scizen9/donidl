pro IR_Widgets3,w,Command,Ret_Val,var1,var2


; This procedure handles many widget tasks
; in the IMGroam environment.  It is not useful by itself.

; **** Determine Boxsize **************************************
  if (Command eq 'WhatSizeBox') then begin
    w1=widget_base(title='Surface Plot size',xoffset=200, $
      yoffset=400,/column)

    sz=[20,40,50,60,75,100,200]

    wbut1=lonarr(7)
    wbut1(0)=widget_button(w1,value='                 20 x 20                 ')
    wbut1(1)=widget_button(w1,value='40 x 40')
    wbut1(2)=widget_button(w1,value='50 x 50')
    wbut1(3)=widget_button(w1,value='60 x 60')
    wbut1(4)=widget_button(w1,value='75 x 75')
    wbut1(5)=widget_button(w1,value='100 x 100')
    wbut1(6)=widget_button(w1,value='200 x 200')

    widget_control,w1,/realize,/show
    event=WIDGET_EVENT(w1)

    for i=0,n_elements(wbut1)-1 do $
      if (event.ID eq wbut1(i)) then Ret_Val=sz(i)

    widget_control,w1,/destroy
    return
    endif


; **** Determine Profile **************************************
  if (Command eq 'WhatProfile') then begin
    w1=widget_base(title='Profile size',xoffset=200, $
      yoffset=400,/column)

    sz=[0,512,128,1,-512,-128]

    wbut1=lonarr(6)
    wbut1(0)=widget_button(w1,value='       X direction - whole width         ')
    wbut1(1)=widget_button(w1,value='X direction - 512 wide')
    wbut1(2)=widget_button(w1,value='X direction - 128 wide')
    wbut1(3)=widget_button(w1,value='Y direction - whole height')
    wbut1(4)=widget_button(w1,value='Y direction - 512 high')
    wbut1(5)=widget_button(w1,value='Y direction - 128 high')

    widget_control,w1,/realize,/show
    event=WIDGET_EVENT(w1)

    for i=0,n_elements(wbut1)-1 do $
      if (event.ID eq wbut1(i)) then Ret_Val=sz(i)

    widget_control,w1,/destroy
    return
    endif


  return
end
