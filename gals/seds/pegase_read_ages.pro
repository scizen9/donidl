pro pegase_read_ages,file_in,tabage,nages
;+
;    reads age table in file_in
;-
ligne1=dblarr(10)
ligne2=dblarr(9)
openr,myunit,file_in,/get_lun
; lecture de l'entete du file_in
chaine=' '
while (strmid(chaine,0,10) ne '**********') do begin
    readf,myunit,chaine
endwhile
;lecture des tables de longueurs d'onde
Nb=intarr(3)
readf,myunit,Nb                      ; Nb contient [ nb de pas de temps, nb de longueurs d'onde du continuum, nb de longueurs d'onde des raies]
nages=Nb[0]
tabage=fltarr(nages)

wlc=fltarr(Nb[1])               ; table des longueurs d'onde du continu
wll=fltarr(Nb[2])               ; table des longueurs d'onde des raies d'emmision
flux=dblarr(Nb[1])
Blockluml=dblarr(Nb[2])
readf,myunit,wlc
readf,myunit,wll

for i=0,nages-1 do begin
    readf,myunit,ligne1
    tabage(i)=ligne1[0]
    readf,myunit,ligne2
    readf,myunit,flux
    readf,myunit,Blockluml
endfor
close,myunit
free_lun,myunit
end
;--------------------------------------------------
