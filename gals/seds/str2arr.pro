;+
; Project     : SOHO - CDS     
;                   
; Name        : STR2ARR()
;               
; Purpose     : Given a delimiter, convert a string to an array of strings. 
;               
; Explanation : Recursively parses the input string using the default or
;               supplied delimiter and creates an array of the elements.
;               
; Use         : IDL> array = strarr( string [, delim=delim, /list, /nomult]) 
;
;               eg print,str2arr(!path,':',/list,/nomult)
;    
; Inputs      : string  - string to be parsed and separated into delimited
;                         components.
;               
; Opt. Inputs : delim   - character defining the delimiter (default = comma)
;               
; Outputs     : array   - string array, number elements=number of delimiters+1
;               
; Opt. Outputs: If /list keyword is present, the array is printed as a list.
;               
; Keywords    : list  - if present the array elements are listed on the screen.
;               nomult - eliminates multiple, consecutive occurrences of the 
;                        deliminter.  ie for a delimiter of <space> then
;                        a consecutive run of n spaces is treated as a single
;                        delimiter.
;
; Calls       : None
;               
; Restrictions: None
;               
; Side effects: None
;               
; Category    : Utilities, Strings
;               
; Prev. Hist. : Yohkoh routine by Sam freeland, LPARL
;
; Written     : CDS version by C D Pike, RAL, 2-Jul-1993
;               
; Modified    : /nomult keyword,  CDP, 17-Nov-93
;
; Version     : Version 2,  17-Nov-1993
;-            

function str2arr,text, delim, $
                 array=array, delimit=delimit, list=list, nomult=nomult

;
;  if listing is required then save for recursion
;
if keyword_set(list) then do_list = 1 else do_list=0

;
;  if cutting multiple delimiters is required then save for recursion
;
if keyword_set(nomult) then do_mult = 1 else do_mult=0

;
; if delimiter supplied then save it for recursion
;
if n_params() eq 2 then delimit=delim

;
;  use comma as default delimiter
;
if not keyword_set(delimit) then delimit=','
delim_len=strlen(delimit)

if n_elements(array) eq 0 then array=''

;
;  find delimiter
;
occur=strpos(text,delimit)

;
;  was a delimiter found?
;
if occur(0) ne -1 then begin

;
; delimiter was found, concatenate and recurse
;
   element=strmid(text,0,occur)	
   array=[array,element]
;
;  recursive call until finished
;
   return,str2arr(strmid(text,occur+delim_len,strlen(text)), $	
                  array=array,delimit=delimit,list=do_list,nomult=do_mult )

;
;  no delimiter found so return original
;
endif else array=[array,text]

;
;  kill those that consist of null stings, they indicate a multiple,
;  consecutive delimiter.
;
if keyword_set(nomult) then begin
   nok = where(array ne '') 
   if nok(0) ge 0 then array = array(nok) else array = array(1:*)
endif else begin
   array = array(1:*) 
endelse

;
;  list if requested
;
if keyword_set(list) then for i=0,n_elements(array)-1 do print,array(i)

return,array 

end 
