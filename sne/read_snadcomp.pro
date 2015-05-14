pro read_snadcomp,sn,choice,silent=silent
;
fil=!SNE_DATA+'/snadcompare.log'
readcol,fil,sn,choice,format='a,a',skip=1,silent=silent
;
return
end
