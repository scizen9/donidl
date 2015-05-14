pro xmessage,msg,buttons,Ret_Val
;+
;NAME:
;      XMESSAGE
;PURPOSE:
;      Routine to pop up little widget dialog boxes.  Each box contains
;      a message and some buttons.  The user sees the message and chooses
;      a button, the index of which is returned to the calling procedure.
;CALLING SEQUENCE:
;      XMESSAGE,message,buttons_array,Ret_Pushed_button
;INPUTS:
;      MESSAGE  - A scalar string of the message.  The '\' indicates a
;                 forced line break, and the message is parsed into
;                 ~50 character lines by xmessage.
;      BUTTONS  - A vector of buttons labels (e.g. ['OK','CANCEL','HELP'] )
;OUTPUTS:
;      RET_PUSH - Index of the button the user pushed (first button is 0)
;PROCEDURE:
;      Message is parsed and displayed with the provided buttons.  User
;      pushes a button and the index is returned.  This procedure is
;      intentionally flexible.  All sorts of goodies can be thought up.
;EXAMPLES:
;   xmessage,'You must select at least 3 stars before you can create an '+ $
;     'astrometric solution',['OK','CANCEL','Try Anyway'],Ret_Val
;
;   xmessage,'File '+filename+' already exists!  Do you want to: ', $
;     ['Replace','View File','CANCEL'],Ret_Val
;MODIFICATION HISTORY
;      05-JUN-94  Written by Eric W. Deutsch
;-

  if (n_params(0) lt 3) then begin
    print,'CALL> XMESSAGE,message,buttons_array,Ret_Pushed_button'
    print,"e.g.> XMESSAGE,'File exists!',['REPLACE','CANCEL'],Ret_Val"
    return
    endif

  msglines=strarr(50) & offset=0 & offtmp=0 & row=0 & flag=0 & brkflag=0

; Sift through the message string and parse into individual lines.  Break
; lines at first space after 50 characters, with safety line-limit of 65 chars.

  while (flag eq 0) do begin
    tmp=strmid(msg,offset+offtmp,1)
    if (strlen(tmp) ne 1) then flag=1 $
    else begin
      if (tmp eq '\') then begin
        msglines(row)=strmid(msg,offset,offtmp)
        row=row+1 & offset=offset+offtmp+1 & offtmp=0 & brkflag=0
        endif
      if (tmp eq ' ') and (offtmp gt 50) and (brkflag eq 0) then brkflag=1
      if (offtmp gt 65) then brkflag=1
      if (tmp ne ' ') and (tmp ne '\') and (brkflag eq 1) then begin
        msglines(row)=strmid(msg,offset,offtmp)
        row=row+1 & offset=offset+offtmp & offtmp=0 & brkflag=0
        endif
      endelse
    offtmp=offtmp+1
    endwhile
  msglines(row)=strmid(msg,offset,offtmp) & row=row+1
  msglines=msglines(0:row-1)

; Create the message part of the widget

  w1=widget_base(/column,title=' ',xoffset=200,yoffset=400)
  w2=lonarr(n_elements(msglines))
  for i=0,row-1 do w2(i)=widget_label(w1,value=msglines(i))

  w3=widget_base(w1,/row)

; Make the buttons

  w4=lonarr(n_elements(buttons))
  for i=0,n_elements(buttons)-1 do $
    w4(i)=widget_button(w3,value='  '+buttons(i)+'  ')

; Show the widget and get the user to push something

  widget_control,w1,/realize

  Ret_Val=-1
  event=WIDGET_EVENT(w1)

  for i=0,n_elements(buttons)-1 do if (event.ID eq w4(i)) then Ret_Val=i

  widget_control,w1,/destroy

  return
end
