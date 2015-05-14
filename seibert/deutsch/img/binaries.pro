
:+
; A sample program to fool around with binaries star systems
:-

; ----------------------------------------------------------------------
print,'Define some useful constants.  All units are in cgs'

G=6.6725985d-8
c=2.99792458d10
h=6.626075540d-27
e=4.80d-10
me=9.109389754d-28
meeV=0.5109990615d
mp=1.672623110d-24
k=1.38d-16
Rh=13.605698140d

Msun=1.99d33
Rsun=6.96d10
Lsun=3.90d33

Mearth=5.98d27
Rearth=6.37d8

AU=1.50d13
pc=3.09d18
yr=60d*60*24*365.24

; ----------------------------------------------------------------------
print,'A sample calculation with the earth and sun

m1=Msun
m2=Mearth
r=AU

r1=m2/(m1+m2)*r
r2=m1/(m1+m2)*r

P=2*!dpi * sqrt( (r1+r2)^3 / (G*(m1+m2)) )
print,'Period=',P/yr,' yr'

v1=2*!dpi*r1/P
print,'v1(sin i)=',v1,' cm/s'

v2=2*!dpi*r2/P
print,'v2(sin i)=',v2/1e5,' km/s'

; ----------------------------------------------------------------------
print,'A sample calculation with a 1.4 Msun NS and 0.073 Msun WD

m1=1.4*Msun
m2=0.073*Msun
r=0.19*Rsun

r1=m2/(m1+m2)*r
r2=m1/(m1+m2)*r

P=2*!dpi * sqrt( (r1+r2)^3 / (G*(m1+m2)) )
print,'Period=',P/60,' min, ', P,' sec'

v1=2*!dpi*r1/P
print,'v1(sin i)=',v1/1e5,' km/s'

v2=2*!dpi*r2/P
print,'v2(sin i)=',v2/1e5,' km/s'


; ----------------------------------------------------------------------
print,'A sample calculation with a 1.2 Msun NS and 1.4 Msun dwarf

m1=1.4*Msun
m2=0.8*Msun
r=4.4*Rsun
r=2.8*Rsun

r1=m2/(m1+m2)*r
r2=m1/(m1+m2)*r

P=2*!dpi * sqrt( (r1+r2)^3 / (G*(m1+m2)) )
print,'Period=',P/3600,' hr'

v1=2*!dpi*r1/P
print,'v1(sin i)=',v1/1e5,' km/s'

v2=2*!dpi*r2/P
print,'v2(sin i)=',v2/1e5,' km/s'


; ----------------------------------------------------------------------
print,'A sample calculation with a 1.4 Msun NS and 0.5 Msun dwarf

m1=1.4*Msun
m2=0.5*Msun
r=2.0*Rsun

r1=m2/(m1+m2)*r
r2=m1/(m1+m2)*r

P=2*!dpi * sqrt( (r1+r2)^3 / (G*(m1+m2)) )
print,'Period=',P/3600,' hr'

v1=2*!dpi*r1/P
print,'v1(sin i)=',v1/1e5,' km/s'

v2=2*!dpi*r2/P
print,'v2(sin i)=',v2/1e5,' km/s'


; ----------------------------------------------------------------------
print,'A sample calculation with two 0.5Msun stars separated by 1 AU

m1=0.5*Msun
m2=0.5*Msun
r=AU

r1=m2/(m1+m2)*r
r2=m1/(m1+m2)*r

P=2*!dpi * sqrt( (r1+r2)^3 / (G*(m1+m2)) )
print,'Period=',P/yr,' yr'

v1=2*!dpi*r1/P
print,'v1(sin i)=',v1/1e5,' km/s'

v2=2*!dpi*r2/P
print,'v2(sin i)=',v2/1e5,' km/s'


; ----------------------------------------------------------------------
print,'A sample calculation with two 1 Msun stars separated by 1 AU

m1=Msun
m2=Msun
r=AU

r1=m2/(m1+m2)*r
r2=m1/(m1+m2)*r

P=2*!dpi * sqrt( (r1+r2)^3 / (G*(m1+m2)) )
print,'Period=',P/yr,' yr'

v1=2*!dpi*r1/P
print,'v1(sin i)=',v1/1e5,' km/s'

v2=2*!dpi*r2/P
print,'v2(sin i)=',v2/1e5,' km/s'



; ----------------------------------------------------------------------
; Equations
;
;             m2
;     r1 = ------- * r
;          (m1+m2)
;     
;     
;     m1 * r1 = m2 * r2
;     
;     
;     F = m * v^2 / r
;     
;     
;          G m1 m2
;     F = ---------
;            r^2
;     
;     
;           2 * pi * r1
;     P = --------------
;           v1 * sin i
;     
;     
;     r1   v1
;     -- = --
;     r2   v2
;     
;     
;           P
;     r1 = ---- * v1 * sin i
;          2 pi
;     
;     
;     Kepler's 3rd law:
;     
;      G (m1 + m2)   (  2 pi )^2
;      ----------- = (-------)
;      (r1 + r2)^2   (   P   )



