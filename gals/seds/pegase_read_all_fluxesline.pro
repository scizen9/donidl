pro pegase_read_all_fluxesline,file_in,flux,fluxline
;+
; output: flux=continuum flux(nbpas de temps,nblongueurs d'onde)
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
wlc=fltarr(Nb[1])               ; table des longueurs d'onde du continu
wll=fltarr(Nb[2])               ; table des longueurs d'onde des raies d'emmision

unflux=dblarr(Nb[1])
flux=dblarr(Nb[0],Nb[1])

unfluxline=dblarr(Nb[2])
fluxline=dblarr(Nb[0],Nb[2])

readf,myunit,wlc
readf,myunit,wll

for i=0,Nb(0)-1 do begin
    readf,myunit,ligne1
    readf,myunit,ligne2

    readf,myunit,unflux
    flux(i,*)=unflux

    readf,myunit,unfluxline
    fluxline(i,*)=unfluxline
endfor

close,myunit
free_lun,myunit

end

;--------------------------------------------------------------------------
