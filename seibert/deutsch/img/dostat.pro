pro dostat,filename

;+
; Procedure: DoStat,filename
;
;  This procedure returns statistics of the output of DoPHOT in the COMPLETE
; format only!  The number of different object types are counted up and the
; results are printed to the screen.
;
;-

  if (n_params(0) ne 1) then begin
    print,"Call> DoStat,'filename.objout'"
    return
    endif

  openr,1,filename
  i=0 & idx=0 & typ=0 & objtypes=intarr(20)
  while not EOF(1) do begin
    readf,1,format='(i6,i3)',idx,typ
    objtypes(typ)=objtypes(typ)+1
    endwhile
  close,1

  desc=['','Single good stars','Galaxies','Siblings of a star group', $
    'Unable to converge on magnitude measurement', $
    'Objects with too few pixels to determine shape', $
    'Too few pixels to be fit using typical shape parameter', $
    'Faint objects for which the shape was not determined', $
    'Objects which could not be modeled and have been excised', $
    'Unable to converge on shape']

  for i=1,9 do begin
    print,format='(a,i2,a,i4,a)','Type: ',i,'   ',objtypes(i),'   '+desc(i)
    endfor
  for i=10,19 do begin
    if (objtypes(i) ne 0) then print,format='(a,i2,a,i4,a)','Type: ',i, $
      '   ',objtypes(i),'   FIXPOS: '+desc(i-10)
    endfor

  print,format='(a,i5)','   Total: ',total(objtypes)

end

