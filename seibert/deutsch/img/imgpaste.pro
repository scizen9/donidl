pro IMGpaste,srcimg,dstimg,xcen,ycen,diam

;+
; NAME:
; IMGpaste
; PURPOSE:
;  This procedure takes a portion from SRCIMG and pastes it into DSTIMG.
;  This is originally intended for replacing a star or other goof that
;  IMGclean has made.  DIAM should usually be odd for a symmetrical
;  replacement.  If DIAM is even, the extra pixels will spill over to the
;  lower x,y pixels.
; CALLING SEQEUNCE:
;  IMGpaste,srcimg,dstimg,xcen,ycen,diam
; HISTORY:
;  07-JAN-93  Written by Eric W. Deutsch
;  11-FEB-93  Bug fix in DIAM determination by Eric W. Deutsch
;-

  if (n_params(0) lt 5) then begin
    print,'Call> IMGpaste,srcimg,dstimg,xcen,ycen,diam'
    print,'e.g.> IMGpaste,imgorig,imgcln,290,350,11'
    return
    endif

  if (n_params(0) lt 5) then diam=11

  diam=fix(diam+.5)
  low=diam/2 & ext=diam-low-1

  dstimg(xcen-low:xcen+ext,ycen-low:ycen+ext)= $
    srcimg(xcen-low:xcen+ext,ycen-low:ycen+ext)

  return

end

