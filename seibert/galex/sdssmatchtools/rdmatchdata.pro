function rdmatchdata, var, sdss=sdss, galex=galex, $
                      grelease=grelease, mis=mis, ais=ais

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reads in a list of columns from the matched GALEX-SDSS photometric
; concatenated catalogs and places them into a structure.
; 
; Defaults to the IR1-DR2 version of the catalogs.
; Currently only supports MIS catalogs.
;
; Syntax: 
;
;  a=rdmatchdata(varlist, /sdss, /galex, /mis, /ais, grelease=)
;    optional keyword grelease = 'ir1' or 'ir0.9' (default = 'ir1') 
;
; Example calls:
; 
;  s=rdmatchdata(['dereg_g','dered_r'], /sdss, /mis, grelease='ir1')
;  g=rdmatchdata(['nuv_mag_dr','fuv_mag_dr'], /galex, /mis, grelease='ir1')

;
; requires funtion file_exist.pro
;
; v1.0; Mark Seibert 11/26/2004
;

;;;;;;;;;
;defaults 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;paths for data - edit these for local environment
; mispath and aispath are assumed subdirectories of 
; ir1path or ir09path and are the location of idl
; sav files for each column (i.e. path=ir1path+mispath)

ir1path='/home/runes9/mseibert/galex-sdss/ir1-dr2/data/'
ir09path='/home/runes9/mseibert/galex-sdss/ir0.9-dr2/data/'

mispath='mis/match-catalogs/concat/allfields/'
aispath='ais/match-catalogs/concat/allfields/'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;error checks and more defaults

if keyword_set(galex) then pre ='g_'
if keyword_set(sdss) then pre ='s_'
if not keyword_set(grelease) then grelease ='ir1'

if not keyword_set(galex) and not keyword_set(sdss) then begin
   print,"ERROR: must specify /galex or /sdss."
   print,"Syntax: a=rdmatchdata(varlist, /sdss, /galex, /mis, /mis, [grelease=,)"
   print,"Example: a=rdmatchdata(['nuv_mag','fuv_mag'], /sdss, /mis, grelease='ir1')"
   print,"         default grelease='IR1'"
   return,0
endif
 
if keyword_set(galex) and keyword_set(sdss) then begin
   print,"ERROR: must specify /galex or /sdss but not both."
   print,"Syntax: a=rdmatchdata(varlist, /sdss, /galex, /mis, /mis, [grelease=,)"
   print,"Example: a=rdmatchdata(['nuv_mag','fuv_mag'], /sdss, /mis, grelease='ir1')"
   print,"         default grelease='IR1'"
   return,0
endif
 

if keyword_set(grelease) then begin
 grel=strcompress(strlowcase(grelease),/rem)
 if grel eq 'ir1' then $
    cpath1=ir1path
 if grel eq 'ir0.9' then $
    cpath1=ir09path
 if grel ne 'ir1' and grel ne 'ir0.9' then begin
   print,"ERROR: must specifiy 'IR1' or 'IR0.9' grelease."
   print,"Syntax: a=rdmatchdata(varlist, /sdss, /galex, /mis, /mis, [grelease=,)"
   print,"Example: a=rdmatchdata(['nuv_mag','fuv_mag'], /sdss, /mis, grelease='ir1')"
   print,"         default grelease='IR1'"
   return,0
 endif
endif

if keyword_set(mis) then $
  cpath2=mispath
if keyword_set(ais) then $
  cpath2=aispath

if keyword_set(mis) and keyword_set(ais) then begin
   print,"ERROR: must specify /mis or /ais but not both."
   print,"Syntax: a=rdmatchdata(varlist, /sdss, /galex, /mis, /mis, [grelease=,)"
   print,"Example: a=rdmatchdata(['nuv_mag','fuv_mag'], /sdss, /mis, grelease='ir1')"
   print,"         default grelease='IR1'"
   return,0
endif

;;;;;;;;;;;;;;;;;;;;;
;restore data columns

cpath=cpath1+cpath2

names='['
vals=","

print,'RDMATCHDATA: retrieving data'

for i=0, n_elements(var)-1 do begin

 var1=strlowcase(strcompress(var[i],/rem))
 file=cpath+pre+var1+'.sav'
 if file_exist(file) then begin
   restore,file
   names=names+"'"+var1+"',"
   cmd='sz=size('+pre+var1+')'
   result=execute(cmd)
   if sz[0] eq 2 then vals=vals+pre+var1+"[*,0]," else $
      vals=vals+pre+var1+"[0],"
 endif else begin
   print,"ERROR: '"+var[i]+"' does not exist in selected catalog."
   return,0 
 endelse

endfor

;;;;;;;;;;;;;;;;;;;;;;
;strip the final comma

names=strmid(names,0,strlen(names)-1)+']'
vals=strmid(vals,0,strlen(vals)-1)

print,'RDMATCHDATA: building structure'

;;;;;;;;;;;;;;;;
;build structure

cmd="g=create_struct("+names+vals+")"
result=execute(cmd)

result=execute('sz=size('+pre+var1+')')
if sz[0] eq 2 then  cmd2='g=replicate(g,n_elements('+pre+var1+'[0,*]))' $
 else cmd2='g=replicate(g,n_elements('+pre+var1+'))'

result=execute(cmd2)

;;;;;;;;;;;;;;;
;fill structure

for i=0, n_elements(var)-1 do begin
 cmd3='g.(i)='+pre+strlowcase(strcompress(var[i],/rem))
 result=execute(cmd3)
endfor

print,'RDMATCHDATA: finished'

return, g

end
