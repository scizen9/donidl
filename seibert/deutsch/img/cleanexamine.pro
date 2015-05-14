pro cleanexamine,imgorig,img,cr
;+
; NAME:
;   CLEANEXAMINE
; PURPOSE:
;   This procedure is designed to aid in the examination of the results of
;   of a run of IMGclean, although there are other uses for the program.
;   In addition to "examining" the difference between the two images, one
;   can edit and replace to remove CR's that IMGclean missed and also fix
;   mistakes where IMGclean damaged a star.
; EXAMPLE:
;   IDL> IMGread,imgorig,horig,'w0c0401t.c0h',2
;   IDL> img=imgorig & h=horig
;   IDL> IMGclean,img,h,cr
;   IDL> cleanexamine,imgorig,img,cr
;
; HISTORY
;   93-APR-01  Finally added header to this procedure.  Eric Deutsch
;   97-MAY-27  Complete rework.  Eric Deutsch
;-

  if (n_params(0) lt 2) then begin
    print,'Call> cleanexamine,original_image,cleaned_image,[crflag_image]'
    print,'e.g.> cleanexamine,imgorig,img,cr'
    return
    endif


  sz=size(img) & sz2=size(imgorig)
  if (sz(1) ne sz(2)) or (sz(1) ne sz2(1)) or (sz(2) ne sz2(2)) then begin
    print,'Both images must be the same size and square.  sorry.'
    return
    endif


  window,0,xs=400,ys=400,title='Original Image',xpos=315,ypos=500	; +320-475
  window,1,xs=400,ys=400,title='Cleaned Image',xpos=735,ypos=500	; +740-475
  if (n_elements(cr) gt 0) then $
  window,3,xs=400,ys=400,title='Flag Image',xpos=740-5,ypos=38+25	; +740-38
  window,4,xs=100,ys=100,title='Zoom Window',xpos=200,ypos=800
  window,2,xs=450,ys=200,title='Input Window',xpos=270-5,ypos=238+25	; +270-237
  datawins=[0,1,3]
  for i=1,4 do begin
    plots,[0,449],[i*50,i*50],/device
    plots,[i*50,i*50],[0,199],/device
    endfor
  plots,[202,202],[0,199],/device,thick=3
  plots,[302,302],[0,199],/device,thick=3
  xyouts,220,20,'!17QUIT',/device,charsize=2
  xyouts,220,70,'NEXT',/device,charsize=2
  xyouts,215,120,'SCALE',/device,charsize=2
  xyouts,220,170,'ZOOM',/device,charsize=2

  xyouts,320,20,'5x5 back',/device,charsize=2
  xyouts,320,70,'3x3 back',/device,charsize=2
  xyouts,315,120,'1 pixfix',/device,charsize=2
  xyouts,320,170,'3x3 clean',/device,charsize=2


  tp1=!d.n_colors-1<255
  fac=sz(1)/200.0
  x=0 & y=0
  skyline,img(50,*),s1,s2
  maxm=s1+200 & skyv=s1 & logflag=1
  flag=0
  print,'Click desired function in Input Window'

  smallim=congrid(bytscl(alog(img-skyv>1),0,alog(maxm),top=tp1-1),200,200)
  smallsz=200.	; physical size of the thumbnail image
  expsz=200.	; number of pixels in the blowup
  wsz=400.	; physical size of the blowup windows
  imsz=sz(1)*1.	; physical size of the image

  while (flag eq 0) do begin

    wset,0
    if (logflag eq 1) then $
      tv,congrid(bytscl(alog(imgorig(x:x+expsz-1,y:y+expsz-1)-skyv>1), $
        0,alog(maxm),top=tp1-1),wsz,wsz) $
    else $
      tv,congrid(bytscl(imgorig(x:x+expsz-1,y:y+expsz-1), $
        skyv,maxm,top=tp1-1),wsz,wsz)

    wset,1
    if (logflag eq 1) then $
      tv,congrid(bytscl(alog(img(x:x+expsz-1,y:y+expsz-1)-skyv>1), $
        0,alog(maxm),top=tp1-1),wsz,wsz) $
    else $
      tv,congrid(bytscl(img(x:x+expsz-1,y:y+expsz-1), $
        skyv,maxm,top=tp1-1),wsz,wsz)

    if (n_elements(cr) gt 0) then begin
      wset,3
      tv,congrid(bytscl(cr(x:x+expsz-1,y:y+expsz-1), $
        0,255,top=tp1),wsz,wsz)
      endif
    wset,2
    tv,smallim
    plots,[0,1,1,0,0]*(expsz/imsz)*smallsz + x/imsz*smallsz, $
      [0,0,1,1,0]*(expsz/imsz)*smallsz + y/imsz*smallsz,/device

    cursor,mx,my,/device

    if (mx lt 200) then begin
      x=fix(mx/smallsz*imsz-expsz/2)<(imsz-expsz)>0
      y=fix(my/smallsz*imsz-expsz/2)<(imsz-expsz)>0
      goto,DONE1
      endif

    if (mx lt 300) then begin
      if (fix(my/50) eq 0) then begin
        flag=1
        endif
      if (fix(my/50) eq 1) then begin
        if (x eq (imsz-expsz)) then begin
          y=fix((y+expsz*.9)<(imsz-expsz))
          x=0
          endif $
        else x=fix((x+expsz*.9)<(imsz-expsz))
        endif
      if (fix(my/50) eq 2) then begin
        print,'Current Scale (Logflag,Sky,Max): ',vect([logflag,skyv,maxm])
        read,'Enter new Logflag,Sky,Max: ',logflag,skyv,maxm
        endif
      if (fix(my/50) eq 3) then begin
        print,'Zoom in which image (Orig,Cleaned): [0,1]'
        key=get_kbrd(1)
        if (key ge '0') and (key le '1n') then begin
          wset,fix(key)
          zoom
          endif
        endif
      goto,DONE1
      endif

    if (mx lt 450) then begin
      if (fix(my/50) eq 0) or (fix(my/50) eq 1) then begin
        print,'Place cursor at center of position to replace in ORIGINAL window'
        print,'  Press [LEFT] mouse button to replace. [RIGHT] to quit'
        print,'  Press [CENTER] mouse button to expand the box.'
        wait,1
        wset,0 & device,set_graphics=6 & device,cursor_image=intarr(16)
        zoom=2
        radius=2-fix(my/50) & curx=200 & cury=200
        boxx=[-1,1,1,-1,-1]*radius*zoom+curx
        boxy=[-1,-1,1,1,-1]*radius*zoom+cury
        for win=0,2 do begin & wset,datawins(win) & plots,boxx,boxy,/device & endfor
        loopflag=0
        while (loopflag eq 0) do begin
          !mouse.button=0
          while (!mouse.button eq 0) do begin
            wset,0
            cursor,curx,cury,/device,/change
            if (fix(curx)/2 eq fix(curx)/2.0) then curx=curx+1
            if (fix(cury)/2 eq fix(cury)/2.0) then cury=cury+1
            for win=0,2 do begin & wset,datawins(win) & plots,boxx,boxy,/device & endfor
            boxx=[-1,1,1,-1,-1]*(radius*zoom+1.5)+curx
            boxy=[-1,-1,1,1,-1]*(radius*zoom+1.5)+cury
            for win=0,2 do begin & wset,datawins(win) & plots,boxx,boxy,/device & endfor
            wset,0 & device,set_graphics=3 & tmp1=tvrd((curx-10)>0<379,(cury-10)>0<379,20,20)
            wset,4 & tv,rebin(tmp1,100,100,/sample) & device,set_graphics=6
            endwhile
          if (!mouse.button eq 1) then begin
            imgpaste,imgorig,img,curx/zoom+x,cury/zoom+y,radius*2+1
            wset,1 & device,set_graphics=3
            tv,congrid(bytscl(alog(img(x:x+expsz-1,y:y+expsz-1)-skyv>1), $
              0,alog(maxm),top=tp1-1),wsz,wsz)
            device,set_graphics=6
            plots,boxx,boxy,/device
            print,'imgpaste,imgorig,img,',curx/zoom+x,cury/zoom+y,radius*2+1
            endif
          if (!mouse.button eq 2) then begin & radius=radius+1 & wait,.3 & endif
          if (!mouse.button eq 4) then loopflag=1
          endwhile
        for win=0,2 do begin & wset,datawins(win) & plots,boxx,boxy,/device & endfor
        wset,0 & device,set_graphics=3 & device,/cursor_crosshair
        endif

      if (fix(my/50) eq 2) then begin
        print,'Place cursor at pixel to fix in CLEANED'
        print,'  window and press left mouse button...'
        wset,1 & cursor,mx1,my1,/device
        img(mx1/2.+x,my1/2.+y)=median(extrac(img,mx1/2.+x-2,my1/2.+y-2,5,5))
        endif

      if (fix(my/50) eq 3) then begin
        print,'Place cursor at center of position to clean in CLEANED window'
        print,'  Press [LEFT] mouse button to clean. [RIGHT] to quit'
        print,'  Press [CENTER] mouse button to expand the box.'
        wait,1
        wset,1 & device,set_graphics=6 & device,cursor_image=intarr(16)
        zoom=2
        radius=1 & curx=200 & cury=200
        boxx=[-1,1,1,-1,-1]*radius*zoom+curx
        boxy=[-1,-1,1,1,-1]*radius*zoom+cury
        for win=0,2 do begin & wset,datawins(win) & plots,boxx,boxy,/device & endfor
        loopflag=0
        while (loopflag eq 0) do begin
          !mouse.button=0
          while (!mouse.button eq 0) do begin
            wset,1
            cursor,curx,cury,/device,/change
            if (fix(curx)/2 eq fix(curx)/2.0) then curx=curx+1
            if (fix(cury)/2 eq fix(cury)/2.0) then cury=cury+1
            for win=0,2 do begin & wset,datawins(win) & plots,boxx,boxy,/device & endfor
            boxx=[-1,1,1,-1,-1]*(radius*zoom+1.5)+curx
            boxy=[-1,-1,1,1,-1]*(radius*zoom+1.5)+cury
            for win=0,2 do begin & wset,datawins(win) & plots,boxx,boxy,/device & endfor
            wset,1 & device,set_graphics=3 & tmp1=tvrd((curx-10)>0<379,(cury-10)>0<379,20,20)
            wset,4 & tv,rebin(tmp1,100,100,/sample) & device,set_graphics=6
            endwhile
          if (!mouse.button eq 1) then begin
            pixedit,img,curx/zoom+x,cury/zoom+y,radius*2+1
            wset,1 & device,set_graphics=3
            tv,congrid(bytscl(alog(img(x:x+expsz-1,y:y+expsz-1)-skyv>1), $
              0,alog(maxm),top=tp1-1),wsz,wsz)
            device,set_graphics=6
            plots,boxx,boxy,/device
            print,'pixedit,img,',curx/zoom+x,cury/zoom+y,radius*2+1
            endif
          if (!mouse.button eq 2) then begin & radius=radius+1 & wait,.3 & endif
          if (!mouse.button eq 4) then loopflag=1
          endwhile
        for win=0,2 do begin & wset,datawins(win) & plots,boxx,boxy,/device & endfor
        wset,1 & device,set_graphics=3 & device,/cursor_crosshair
        endif

      endif


DONE1:

    endwhile

FINISH:

end
