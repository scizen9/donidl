pro IR_KbdHndlr,image,img1,mx,my,KeyHit,hdr

;+
; IR_KbdHndlr,image,img1,mx,my
;
; This is the main keyboard handler for IMGroam.  It calls the procedure
; User_Kbd first and then handles the default operations.  The use of
; common blocks are required to communicate with IMGroam.
;
; Calls: User_Kbd,image,img1,mx,my,KeyHit
;   image  -  whole image
;   img1   -  currently displayed frame
;   mx     -  current x cursor position in whole image 
;   my     -  current y cursor position in whole image
;               center of lower left pixel is (0.00,0.00)
;   KeyHit -  The (first) character hit.  Note that there may be more
;               characters in the keyboard buffer and the first
;               character could be an ESC, the first character of an
;               escape sequence (e.g. up arrow).
;   When adding a function to User_Kbd, be sure that the procedure does
;   not disturb the value in KeyHit unless it processes the Key Hit, at
;   which point it should set KeyHit='' so that if you are overriding the
;   default key calls, they are not called, too...
;   Remember to take case sensitivity into account!
;   The keyboard buffer is flushed after both the User and default handler
;   runs.
;-

; Call the user-defined handler first so others can override
  User_Kbd,image,img1,mx,my,KeyHit,hdr

; *** Print a List of available keys *********************************
  if (KeyHit eq '?') then begin
    print,' '
    print,'Available Keystrokes:'
    print,' a  Aperture Photometry'
    print,' c  Cosmic-ray fix'
    print,' e  extract DIS spectrum'
    print,' f  find DIS slit (vertical gaussian fit)'
    print,' i  Image stats in a 20x20 box'
    print,' p  Profile plots'
    print,' r  Replace pixel values with values in another'
    print,' s  Surface plots'
    KeyHit=''
    endif

; *** Cosmic Ray Pixel Editor ************************************
  if (strupcase(KeyHit) eq 'C') then begin
    pixedit,image,mx,my
    irdisp,image,img1,3
    KeyHit=''
    endif

; *** Cosmic Ray Pixel Replacer ************************************
  if (strupcase(KeyHit) eq 'R') then begin
    pixedit2,image,mx,my
    irdisp,image,img1,3
    KeyHit=''
    endif

; *** 20x20 Image Statistics ************************************
  if (strupcase(KeyHit) eq 'I') then begin
    imstat,image,mx,my
    KeyHit=''
    endif

; *** Image Surface Plots ************************************
  if (strupcase(KeyHit) eq 'S') then begin
    IR_widgets3,w,'WhatSizeBox',Ret_Val
    xsurface,extrac(image,mx-Ret_Val/2,my-Ret_Val/2,Ret_Val,Ret_Val)
    KeyHit=''
    endif

; *** Image Profile Plots ************************************
  if (strupcase(KeyHit) eq 'P') then begin
    IR_widgets3,w,'WhatProfile',Ret_Val
    sz=size(image)
    if (Ret_val eq 0) then begin & idx=indgen(sz(1)) & tmp=image(*,my) & endif
    if (Ret_val eq 1) then begin & idx=indgen(sz(2)) & tmp=image(mx,*) & endif
    if (Ret_val gt 5) then begin
      tmp=image((mx-Ret_Val/2)>0:(mx-Ret_Val/2+Ret_Val-1)<(sz(1)-1),my)
      idx=indgen(abs(Ret_Val))+((mx-Ret_Val/2)>0)
      endif
    if (Ret_val lt -5) then begin
      tmp=image(my,(my+Ret_Val/2)>0:(my+Ret_Val/2-Ret_Val-1)<(sz(2)-1))
      idx=indgen(abs(Ret_Val))+((my+Ret_Val/2)>0)
      endif
    pwidget,idx,reform(tmp)
    KeyHit=''
    endif

; *** Find Slit ************************************
  if (strupcase(KeyHit) eq 'F') then begin
    disslit,image,mx,my
    KeyHit=''
    endif

; *** Extract spectrum ************************************
  if (strupcase(KeyHit) eq 'E') then begin
    extractspec,image,mx,my
    KeyHit=''
    endif

BRK:
  return
end
