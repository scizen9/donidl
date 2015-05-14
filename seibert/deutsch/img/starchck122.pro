pro starchck122,img1,x,y,skyv,rms,avg1,below

  COMMON STARCHCK122,SC_inner_width,SC_outer_width,SC_upper_lim

  avg1=[1.,1.] & below=[1,1]
  size=SC_outer_width*2 & tst=extrac(img1,x-size/2,y-size/2,size,size)
  skyline122,tst,skyv,rms
  size=SC_inner_width & tst=extrac(img1,x-size/2,y-size/2,size,size)
  ttmp=where((tst lt skyv+rms*SC_upper_lim) and (tst ne 0))
  chk=size(ttmp)
  if (chk(0) eq 0) then begin
    print,'WARNING: [STARCHCK] area level too high.  Keeping object.'
    avg1=[skyv+rms*50,skyv+rms*10]
    below=[2,5]
    return
    endif
  tst=tst(ttmp)
  inner_elem=n_elements(tst)
  btmp=where(tst lt skyv-rms)
  chk=size(btmp) & below(0)=0
  if (chk(0) ne 0) then below(0)=n_elements(btmp)
  avg1(0)=total(tst)/n_elements(tst)
;  print,total(tst),n_elements(tst)
;  print,tst

  size=SC_outer_width & tst=extrac(img1,x-size/2,y-size/2,size,size)
  ttmp=where((tst lt skyv+rms*SC_upper_lim) and (tst ne 0))
  tst=tst(ttmp)
  outer_elem=n_elements(tst)
  btmp=where(tst lt skyv-rms)
  chk=size(btmp) & below(1)=0
  if (chk(0) ne 0) then below(1)=fix(n_elements(btmp)*(inner_elem*1./outer_elem))
  avg1(1)=total(tst)/n_elements(tst)
;  print,total(tst),n_elements(tst)
;  print,tst

  return
end
