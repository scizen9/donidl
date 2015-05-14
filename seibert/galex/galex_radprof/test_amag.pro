r=(findgen(20)+1)*6

m=15 + 1/(r^0.5); +10

plot,r,m,yr=[max(m),min(m)],psym=-2

dm=m - shift(m,+1)
dr=r-shift(r,+1)

plot,dm[0:18]/dr[0:18],m[0:18],yr=[max(m),min(m)],psym=-2
