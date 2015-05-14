pro IR_Disp,OBJECT,DN

; This procedure handles text display of various information
; in the IMGroam environment.  It is not useful by itself.

  COMMON fparm,NAXIS1,NAXIS2,xsize,ysize,xcent,ycent,xll,yll,zoom,frtyp
  COMMON ANSI,cr,lf,ESC,up,clrscrn,bell
  COMMON IR_ENVIR,stat,itype

  print,clrscrn,'##### CURRENT FRAME STATUS #####'
  print,'OBJECT NAME: ',OBJECT,'    IMAGE SIZE: ',vect([NAXIS1,NAXIS2])
  print,'FRAMESIZE: ',vect([xsize,ysize]),'    FRAME CENTER: ',vect([xcent,ycent])
  print,'LWR-LFT CORNER: ',vect([xll,yll]),'    ZOOM FACTOR: ',strn(zoom)
  print,'SETTINGS: AUTO(DISPLAY,BORDER,CBAR DISPLAY,CBAR BORDER): ', $
    vect([YesNo2(stat.AUTOD,type=6),YesNo2(stat.AUTOB,type=6), $
    YesNo2(stat.CBAUTOD,type=6),YesNo2(stat.CBAUTOD,type=6)])
  
  if (n_params(0) lt 3) then DN=0.
  IR_astdisp,xcent,ycent,ra,dec,DN,coordtype='Center'

  return
end
