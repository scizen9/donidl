pro User_Kbd,image,img1,mx,my,KeyHit,hdr

;+
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

; *** An example ************************************
  if (strupcase(KeyHit) eq 'U') then begin
    ; This is where the code to call your procedure would go.  Actually
    ; putting the code here is discouraged.
    ; KeyHit=''			; Don't forget this...
    tmp=1
    endif

  if (strupcase(KeyHit) eq 'A') then begin
    zpt=25.0 & exptime=1.0 & gain=5.0
    if (strn(sxpar(hdr,'INST')) eq 'grim') then begin
      if (sxpar(hdr,'FILTER1') eq 1) then zpt=23.11
      if (sxpar(hdr,'FILTER1') eq 2) then zpt=23.02
      if (sxpar(hdr,'FILTER1') eq 3) then zpt=22.26
      exptime=sxpar(hdr,'OPENTIME') & if (exptime eq 0.00) then exptime=1.0
      endif
    apers=[3,5,7,9]
    qaper,image,mx,my,cts,cerr,skyv,serr,gain,apers,[15,25,5], $
      [-1000,65000],exptime=exptime,zpt=zpt,/mags
    KeyHit=''			; Don't forget this...
    endif

BRK:
  return
end
