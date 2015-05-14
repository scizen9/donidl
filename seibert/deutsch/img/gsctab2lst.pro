pro gsctab2lst,inputtab,outlst,regionid

;+
; convert a FITS table from IRAF 'regions' task into the ascii list like
; that outputted by the VAXes.  -Deutsch 4/3/94
;
; Call> gsctab2lst,inputtab,outlst
; e.g.> gsctab2lst,'47tuc.tab','47tuc.gsclst'
;
;-

  if (n_params(0) lt 2) then begin
    print,'Usage: gsctab2lst,inputtab,outlst'
    return
    endif

  if (not exist(inputtab)) then begin
    print,'[gsctab2lst]: Error - Unable to open file '+inputtab
    return
    endif


  if (n_elements(regionid) eq 0) then regionid=1000


  tab_read,inputtab,tcb,table,hdr

  ra=tab_val(tcb,table,2)
  dec=tab_val(tcb,table,3)
  mag=tab_val(tcb,table,4)
  gsc_id=tab_val(tcb,table,5)
  plt_id=tab_val(tcb,table,6)
  class=tab_val(tcb,table,10)
  mult=tab_val(tcb,table,11)

  typ=['str','non','non','non']
  nmult=(fix(byte(mult))-70)/14

  openw,1,outlst
  printf,1,' GSC number    RA 2000.0      Dec 2000.0    Mag   Class Mult Plate'
  printf,1,' '

  for i=0,n_elements(ra)-1 do begin
    tmp=adstring(ra(i),dec(i),2)
    tmp=strmid(tmp,0,14)+strmid(tmp,13,99)
    printf,1,format='(i5,a1,a5,2x,a,f8.2,a6,i4,a6)', $
      regionid,'-',strn(gsc_id(i),length=5,padchar='0'), $
      tmp,mag(i),typ(class(i)<3),nmult(i),plt_id(i)
    endfor

  close,1

  return

end




