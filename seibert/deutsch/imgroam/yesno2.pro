function YesNo2,flag,type=type,other=other
;+
; NAME:
;   YESNO2
; PURPOSE:
;   This function returns a string corresponding YES/NO to 1/0.  i.e. if a 1
;   is passed to YESNO2, the string 'YES' or 'Yes' or 'yes' or 'y' is returned.
; CALLING SEQEUNCE:
;   tmp=yesno2(1,type=1)
; INPUT:
;   FLAG      This variable should contain 1 for YES and 0 for NO.
; OPTIONAL KEYWORDS:
;   TYPE      Specifies the mode or type of returned string (default=0)
;      Other equivalents:
;   type= 0    1    2     3    4    5      6     7     8    9   10  11    12
;   F=1 'YES','Y','Yes','yes','y','yup',  'ON', 'On', 'on','X','1','Si' , 'Ja'
;   F=0  'NO','N', 'No', 'no','n','nope','OFF','Off','off',' ','0','Non','Nein'
;   OTHER     Specifies what to return if flag is neither 0 nor 1.
; OUTPUT:
;   tmp       The string containing the answer
; NOTES:
;   none
; HISTORY:
;   02-JUN-92  Older Version of YesNo2 Finished and documented.   E. Deutsch
;-

  if (n_params(0) lt 1) then begin
    print,'Call: IDL> tmp=YesNo2(boolean,[type=])'
    print,'e.g.: IDL> print,YesNo2(1,type=2)'
    return,''
    endif
  if (n_elements(type) eq 0) then type=0
  s=size(other)
  if (s(1) eq 0) then other=0

; type   0    1    2     3    4    5     6    7     8     9   10  11    12
  Yes=['YES','Y','Yes','yes','y','yup', 'ON','On', 'on', 'X','1','Si', 'Ja']
  No= ['NO', 'N','No', 'no', 'n','nope','OFF','Off','off',' ','0','Non','Nein']

  Oth=['??', '', ' ', '???', '?','eh?','????']

  if (flag eq 1) then return,Yes(type)
  if (flag eq 0) then return,No(Type)
  if (s(1) eq 7) then return,other else $
    return,Oth(other)

end
