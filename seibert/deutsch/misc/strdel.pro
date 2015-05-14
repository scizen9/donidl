function strdel,str,pos,len
;+
; NAME:
;   StrDel
; PURPOSE:
;   This function removes the piece specified from a string.  For example,
;     IDL> str='   3,234.99'
;     IDL> print,float(strdel(str,strpos(str,','),1))
;   yields 3234.99
; CALLING SEQEUNCE:
;   tmp = StrDel(string,pos,len)
; INPUT:
;   STRING    This is the string variable from which the subtring it to be
;               removed.
;   POS       First character to remove where the first character is 0.
;   LEN       Number of characters to remove.
; HISTORY:
;   30-AUG-1990 Version 1 written.
;   27-JUL-1992 Proper Header added.
;-

result=strmid(str,0,pos)+strmid(str,pos+len,strlen(str)-1)

return,result
end
