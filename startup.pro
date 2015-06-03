!quiet=1
device,retain=2
udir = 'Users'	; default for Mac OS X
sdir = 'srl'	; how to access linux disks from Mac OS X
if strpos(!version.os,'linux') ge 0 then udir = 'users'
if strpos(!version.os,'linux') ge 0 then sdir = 'users'
loginfo = get_login_info()
unam = loginfo.user_name
;
astrolib
;
defsysv,'!top_color',0
defsysv,'!top_colorx',0
defsysv,'!top_colorps',255
defsysv,'!colorstr',''
defsysv,'!colorstr_ps',''
defsysv,'!colorstr_x',''
;
defsysv,'!PHYS_C',2.99792458d5  ; speed o' light
defsysv,'!COSMO_OM',0.27	; Omega matter
defsysv,'!COSMO_OL',0.73	; Omega Lambda
defsysv,'!COSMO_H0',73.0	; Hubble constant

; photometry setups
defsysv,'!PHOT_DATA','/'+udir+'/'+unam+'/idl/data/kcorr/'
COMMON filter_info,master_filter
read_filter_file
vega_struct=read_vega(/fullwave)
calc_zps,vega_struct
;
defsysv,'!SNE_DATA','/'+udir+'/'+unam+'/Data/sne/'
defsysv,'!GALS_DATA','/'+udir+'/'+unam+'/Data/gals/'
defsysv,'!CAT_DATA','/'+udir+'/'+unam+'/donidl/data/cats/'
defsysv,'!NED_DATA','/'+udir+'/'+unam+'/donidl/data/ned/'
defsysv,'!NED_CACHE','/'+udir+'/'+unam+'/ref/ned/'
defsysv,'!NGA_DATA','/'+udir+'/'+unam+'/Data/nga/'
defsysv,'!2MASS_DATA','/'+udir+'/'+unam+'/Data/2mass/'
defsysv,'!SDSS_DATA','/'+udir+'/'+unam+'/Data/sdss/'
defsysv,'!WISE_DATA','/'+udir+'/'+unam+'/Data/wise/'
defsysv,'!GALEX_DATA','/'+udir+'/'+unam+'/Data/galex/'
;defsysv,'!GALDB_DATA','/'+udir+'/'+unam+'/idl/data/galdb/'
defsysv,'!GLGA_DATA','/'+udir+'/'+unam+'/Data/glga/'
defsysv,'!GLGA_WISE_DATA','/Volumes/scidata/'+unam+'/wise/'
defsysv,'!GLGA_SDSS_DATA','/home/ymir/neill/sdss/'
defsysv,'!GLGA_2MASS_DATA','/disk/nagelfar2/neill/2mass/'
defsysv,'!GLGA_ROOT','/'+udir+'/'+unam+'/glga/'
defsysv,'!GLGA_MS_ROOT','/'+sdir+'/mseibert/glga/'
defsysv,'!ZPEG_ROOT','/'+udir+'/'+unam+'/zpeg_5.17/'
;
; KCWI system variables
defsysv,'!KCWI_DATA','/'+udir+'/'+unam+'/kcwi/pipeline/releases/kderp/data/'
defsysv,'!CWI_DATA','/'+udir+'/'+unam+'/kcwi/pipeline/releases/kderp/cwi/'
;
COMMON sndb_info,sndat
COMMON galdb_info,galdat,gphsrc
COMMON glgadb_info, glgadat
sndb_restore
galdb_restore
galdb_src_read
.run sndb_data__define
.run galdb_data__define
.run glgadb_data__define
glgadb_restore
;
;.compile GALEXSpectrum__define.pro
;.compile GSViewer__define.pro
;.compile TwinViewer__define.pro
;
;gx_snhostan_inter
!quiet=0
