pro rddaohdr,flun,nl,nx,ny,lowbad,highbad,thresh,ap1,phadu,rnoise,frad
;
rec=''
readf,flun,rec
readf,flun,rec
nl = fix(gettok(rec,' '))
nx = fix(gettok(rec,' '))
ny = fix(gettok(rec,' '))
lowbad = float(gettok(rec,' '))
highbad = float(gettok(rec,' '))
thresh = float(gettok(rec,' '))
ap1 = float(gettok(rec, ' '))
phadu = float(gettok(rec,' '))
rnoise = float(gettok(rec,' '))
frad = float(gettok(rec,' '))
readf,flun,rec
;
return
end
