pro IR_cntrd,image,xguess,yguess,xcen,ycen,FWHM1,info=info,mode=mode

;+
; PROCEDURE:
;   IR_cntrd,image,xguess,yguess,xcen,ycen,FWHM,info=info
; PURPOSE:
;   This procedure is called by IMGRoam so as to provide an indepdant
;   method of calling a centroiding algorithm.  Several can be
;   available.
;-

  COMMON IR_cntrd_COM,which_cntrdr

  if (n_elements(which_cntrdr) eq 0) then which_cntrdr=1

  if (n_elements(mode) eq 0) then mode=[0,which_cntrdr]
  if (n_elements(mode) eq 1) then mode=[mode,which_cntrdr]
  if (n_elements(FWHM1) eq 0) then FWHM1=5

  if (mode(0) eq 0) then begin

; ****** Astronomy Library's centroider CNTRD ******************************
    if (which_cntrdr eq 0) then begin
      xgess=fix(xguess+.5)
      ygess=fix(yguess+.5) & info=0
      cntrd,image,xgess,ygess,xcen,ycen,FWHM1
      return
      endif

; ****** Eric Deutsch's centroider BScentrd (optimized for Bad Stars,   ****
;          i.e. small scruffy things.  Works well on regular stars and  ****
;          crowded star regions ********************************************
    if (which_cntrdr eq 1) then begin
      bscentrd,image,xguess,yguess,xcen,ycen,FWHM1,info=info
      return
      endif

; ****** Eric Deutsch's test 2D gaussian fitting centroider starfit ********
    if (which_cntrdr eq 2) then begin
      starfit,image,xguess,yguess,xcen,ycen,FWHM1,info=info
      return
      endif

; ****** Eric Deutsch's pure center of 'mass' within a given box radius ****
    if (which_cntrdr eq 3) then begin
      objcenom,image,xguess,yguess,xcen,ycen,info=info
      return
      endif

    print,'[IR_CNTRD]: Unrecognized Centroider Designation!!!'
    return
    endif

;===============================================================================
  if (mode(0) eq 1) then begin
    w1=widget_base(title='Select Centroider',xoffset=200,yoffset=400,/column)

    w10=widget_base(w1,/row)

    w2=widget_base(w10,/column,/exclusive)
    wbut1=lonarr(5)
    wbut1(0)=widget_button(w2,value='CNTRD (ASTRONLIB)')
    wbut1(1)=widget_button(w2,value='BScentrd (EWD)')
    wbut1(2)=widget_button(w2,value='Starfit (EWD)')
    wbut1(3)=widget_button(w2,value='ObjCenOM (EWD)')
    wbut1(4)=widget_button(w2,value='User Centroider 1')

    w3=widget_base(w10,/column)
    w4=widget_button(w3,value='     OK      ')
    w5=widget_button(w3,value='   CANCEL    ')

    w11=widget_text(w1,xsize=55,ysize=10,/scroll)

    wc=which_cntrdr & flag=0
    widget_control,wbut1(wc),/set_button
    widget_control,w1,/realize,/show

    while (flag eq 0) do begin

      if (wc eq 0) then widget_control,w11,set_value= $
       ['This Centroider is comes standard with the Astronomy library', $
        'from Wayne Landsman at the GSFC.  A good centroider for nice', $
        'well-formed stars.  If the stars in question are small and', $
        'scruffy, BScentrd might be a better choice.  See the help', $
        'entry of CNTRD for more details.']

      if (wc eq 1) then widget_control,w11,set_value= $
       ['This Centroider is optimized to do the best job possible on ', $
        'small, scruffy stars which choke most centroiders.  It also ', $
        'is well designed to centroid objects right next to bigger, ', $
        'brigher objects, to which many centroiders immediately jump.  ', $
        'However, it also yields good results for virtually all stars.  ', $
        'This procedure was written by Eric W. Deutsch.']

      if (wc eq 2) then widget_control,w11,set_value= $
       ['This is a test centroider by Eric Deutsch designed to', $
        'fit a 2D gaussian to the star.  This ought to be better', $
        'than a center-of-mass centroider, but we ll see..']

      if (wc eq 3) then widget_control,w11,set_value= $
       ['This is a test centroider by Eric Deutsch designed to', $
        'simply determine the center of mass (i.e. volume)', $
        'within a given radius that the user must type in', $
        'every time.  This is useful for irregular shapes.']

      if (wc eq 4) then widget_control,w11,set_value= $
       ['This is User Centroider #1, as yet undefined.']

      event=WIDGET_EVENT(w1)
      if (event.ID eq w4) then flag=1
      if (event.ID eq w5) then flag=-1
      for i=0,n_elements(wbut1)-1 do if (event.ID eq wbut1(i)) then wc=i
      endwhile

    widget_control,w1,/destroy
    if (flag eq -1) then return

    which_cntrdr=wc
    return
    endif

  return
end

