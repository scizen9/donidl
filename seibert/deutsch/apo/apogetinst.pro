pro apogetinst,instrument,success
;+
; APO SOFTWARE
;
; will read an instrument block to get details of the instrument and guider
;
;-

  COMMON INSTBLOCK,IIm_Offset,IIm_Scale,IIm_MinXY,IIm_MaxXY,Rot_Inst_xy, $
    Rot_Inst_ang,GIm_Offset,GIm_Scale,GIm_MinXY,GIm_MaxXY, $
    GC_GIm_xy,GC_GIm_ang

  success=-1
  instblockdir='/host/dione/d1/deutsch/apopub/guider/instblocks/'

  if (n_params(0) lt 1) then begin
    print,'Call>  apogetinst,instrument, success'
    print,"e.g.>  apogetinst,'dsc',success"
    return
    endif

  inst=strlowcase(instrument)
  if (inst ne 'dsc') and (inst ne 'grim') and (inst ne 'dis') and (inst ne 'spi') and (inst ne 'gfp') then begin
    print,"'"+inst+"' is not a supported instrument."
    print,'Currently supported instruments are: dsc, grim, dis, spi, gfp'
    return
    endif

  vars=['IIm_Offset','IIm_Scale','IIm_MinXY','IIm_MaxXY','Rot_Inst_xy', $
    'Rot_Inst_ang','GIm_Offset','GIm_Scale','GIm_MinXY','GIm_MaxXY', $
    'GC_GIm_xy','GC_GIm_ang']

  IIm_Offset=[-999,-999]
  IIm_Scale=[-999,-999]
  IIm_MinXY=[-999,-999]
  IIm_MaxXY=[-999,-999]
  Rot_Inst_xy=[-999,-999]
  Rot_Inst_ang=-999

  GIm_Offset=[-999,-999]
  GIm_Scale=[-999,-999]
  GIm_MinXY=[-999,-999]
  GIm_MaxXY=[-999,-999]
  GC_GIm_xy=[-999,-999]
  GC_GIm_ang=-999
  lin=''

  on_ioerror,CANTOPEN
  openr,1,instblockdir+inst+'.instblock'
  print,'Reading instrument block information from:'
  print,instblockdir+inst+'.instblock'
  print,'-------'

  lctr=0 & off=0
  on_ioerror,READERR
  while not EOF(1) do begin
    readf,1,lin & lin=strn(lin)
    if (lctr eq 0) and (strmid(lin,0,2) eq ' !') then off=1
    if (strmid(lin,off,1) ne '!') and (lin ne '') then begin
      keyvar=getwrd(lin,off) & match=-1
      for i=0,n_elements(vars)-1 do if (keyvar eq vars(i)) then match=i
      if (match gt -1) then begin
        if (strpos(keyvar,'ang') ge 0) then tmp=keyvar+'='+getwrd(lin,1) $
        else tmp=keyvar+'=['+getwrd(lin,1)+','+getwrd(lin,2)+']'
        print,'   ',tmp
        result=execute(tmp)
        endif
      endif
    lctr=lctr+1
    endwhile

  close,1

  success=0
  return

CANTOPEN:
  print,'ERROR: ',!ERR_STRING
  print,'Unable to open '+instblockdir+inst+'.instblock'
  print,'Instrument block data not read.'
  return

READERR:
  print,'ERROR: ',!ERR_STRING
  print,'Error while reading '+instblockdir+inst+'.instblock'
  print,'Instrument block data not read.'
  return

end









