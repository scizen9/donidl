pro pegase_read_lambdaline,file_in,nlambda,lambda,nlambdaline,lambdaline
openr,myunit,file_in,/get_lun
; lecture de l'entete du file_in
chaine=' '
while (strmid(chaine,0,10) ne '**********') do begin
 readf,myunit,chaine
endwhile
;lecture des tables de longueurs d'onde
Nb=intarr(3)
readf,myunit,Nb ; Nb contient [ nb de pas de temps, nb de longueurs d'onde du continuum, nb de longueurs d'onde des raies]
nlambda=Nb[1]
lambda=dblarr(Nb[1]) ; table des longueurs d'onde du continu
readf,myunit,lambda 

nlambdaline=Nb[2]
lambdaline=dblarr(Nb[2]) ; table des longueurs d'onde du continu
readf,myunit,lambdaline 
close,myunit
free_lun,myunit
end
;------------------------------
