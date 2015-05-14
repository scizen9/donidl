;+
;NAME
; read_filter_file
;PURPOSE
; reads filter files into the common block filter_info
;USAGE
; read_filter_file
;RETURNS
; Array of filter info

;OPTIONAL INPUTS
;
; filterairmass  Airmass at which to apply atmosphere to filter
;                 response. Megcam filters only. Not applied if noatm
;                 or noconvolve are set. Def: 1.
;
;KEYWORDS
; noconvolve      Don't include megacam system throughput, only filter
;                  functions.  Has no effect on filters that don't
;                  have 'megacam' in their ID.  Note that, despite the
;                  name, including the MegaCam response does not
;                  involve a convolution of any sort, but simply a
;                  multiplication.  This includes the CCD QE, optics,
;                  the primary reflection, and one airmass of 
;                  the Mauna Kea atmospheric transmission.
; noatm           Don't include atmospheric transmission in MegaCam
;                  filters.  Has no effect if /NOCONVOLVE is set
; reread          Normally the filter files are read from a save
;                  file.  If this is set they are reread from a text
;                  file
; nosave          Normally the filter array is saved to a .sav file.
;                  If this keyword is set this is not done.  Note
;                  that this has no effect unless reread is set.
;COMMON BLOCKS
; filter_info     Modified.  Filters are loaded into filter_info:master_filter
;AUTHOR
; Mark Sullivan
;-
PRO read_filter_file,ENERGY=energy,VERBOSE=verbose,NOATM=noatm,$
                     NOCONVOLVE=noconvolve,REREAD=reread,NOSAVE=nosave,$
                     FILTERAIRMASS=filterairmass

; Important note:
;
; Johnson-Cousins system is PHOTON (or counts) based
; Filter transmission S(lambda) is DIMENSIONLESS - same for photons or energy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For energy flux density distributions:
; F(lambda) is in 

; For a photon-based system (type 0):
;   mag=-2.5LOG10( int( lambda*S(lambda)*F(lambda)dlambda))

; For a energy-based system (type 1):
;   mag=-2.5LOG10( int( S(lambda)*F(lambda)dlambda))
;
; see Appendix OF Nugent 2002 http://www.journals.uchicago.edu/PASP/journal/issues/v114n798/202083/202083.html

;;Both the AB and Vegamax systems are photon based

;;Be careful -- this does not correspond to responsetype as defined below.

;;Some authors tabulate lambda * S, where S is the dimensionless
;; transmission, instead of S.  A famous example is Bessell (1990)
;; for his UBVRI.  Because the lambda has already been absorbed
;; a filter curve that has had lambda pre-multiplied is sometimes
;; called energy based, since the integral looks like case 1 above.
;;
;;This code returns lambda * S for convenience.  We handle this as
;; follows: if the filter has responsetype 0, that means that lambda
;; has already been multiplied in, so we don't have to do anything.
;; If it has responsetype 1, then this code will multiply it by
;; lambda.
;;Thus, to get the magnitude using the filter curve that comes
;; back from this function, you _don't_ include the extra lambda.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; For photon flux density e.g. Hamuy 2001 http://www.journals.uchicago.edu/ApJ/journal/issues/ApJ/v558n2/53510/53510.html
;
; N(lambda) = F(lambda)*lambda/hc
;
; Hence, Hamuy divides Bessell responses by lambda before integrating
; up - he uses photon flux density distributions i.e. for type 0 transmissions:
;
; mag=-2.5LOG10( int( S(lambda)*N(lambda)dlambda))
; 
; And it all makes sense!!
;
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

COMMON filter_info, master_filter

IF(N_ELEMENTS(noatm) EQ 0)THEN noatm=0b
IF(N_ELEMENTS(nosave) EQ 0)THEN nosave=0b
IF(N_ELEMENTS(verbose) EQ 0)THEN verbose=0b

;;The idea is to update this (from SYSTIME(1)) whenever the return structure
;; definition is changed so that the code will automatically regenerate the
;; savefiles.  In reality, this won't be kept up to date, but it won't
;; hurt and will help at least some of the time
struct_modtime = 1172765956L ;;Mar 3, 2007, 11:19am

;; maximum allowed number of wavelengths in each filter
nmaxpoints=1400

basedir = !PHOT_DATA
filter_filename = basedir+'/filters.txt'
filter_savefilename=basedir+'/filters.sav'


;;Decide if we are going to read from the save file or the text file
;;In order to restore, the sav file must exists, reread can't
;; be set, and the save file has to be more recent than the 
;; text file.
IF FILE_TEST( filter_savefilename, /READ, /REGULAR ) AND $
  (~ KEYWORD_SET(reread)) THEN BEGIN
    savinfo = FILE_INFO( filter_savefilename )
    IF ~ FILE_TEST( filter_filename, /READ, /REGULAR ) THEN BEGIN
        PRINT,"ERROR in read_filter_file: can't find input file: ",$
              filter_filename
        RETURN
    ENDIF
    txtinfo = FILE_INFO( filter_filename )
    last_change_time = MAX( [txtinfo.mtime, struct_modtime] )
    ;;Test the time of last modification
    IF savinfo.mtime GE last_change_time THEN restoresave=1b ELSE BEGIN
        IF KEYWORD_SET( verbose ) THEN $
          PRINT,"Save file is older than last update -- rereading"
        restoresave=0b
    ENDELSE
ENDIF ELSE restoresave=0b

IF restoresave THEN BEGIN
    RESTORE,filter_savefilename,VERBOSE=verbose
    RETURN
ENDIF

;;Otherwise we're reading the file
A = {id:'', shortid:'', responsetype:0L, vegazp:!VALUES.D_NAN, $
     abzp: !VALUES.D_NAN, ab_offset: !VALUES.D_NAN, bdzp: !VALUES.D_NAN,$
     area_lambda:!VALUES.D_NAN, area_nu: !VALUES.D_NAN, $
     mean_wave:!Values.D_NAN, calibtype:0L, $
     npoints:0L, vega_colour: !VALUES.D_NAN, vegacolsys: '',$
     vega_mag: !VALUES.D_NAN, wave:DBLARR(nmaxpoints), $
     response:DBLARR(nmaxpoints), megacam_included: 0b, $
     megacam_atm_included: 0b, temporary: 0b }

master_filter=REPLICATE(A,1)  

; read in megacam throughput- include CCD QE
read_megacamthroughput,megacam_response_wave,megacam_response,$
  NOATM=noatm,FILTERAIRMASS=filterairmass

OPENR,unit,filter_filename,/GET_LUN

nfilter=-1

;; Loop round filters file, reading them in
;;There are three types of lines
;; Header lines, of the form Filter: calibtype responsetype vegacolor
;;    shortid id
;; Followed by a variable number of lines of the form wave resp
;; There can also be a special line 'micron' which tells the code
;;  to use micron wavelength units
line=''
micron=0b
npoints=0L
WHILE ~ EOF(unit) DO BEGIN

   READF,unit,line
   array=STRSPLIT(line,COUNT=nsplit,/EXTRACT,/REGEX)

; start reading a new filter
   IF (STRCMP(array[0],'Filter:',/FOLD_CASE)) THEN BEGIN
       master_filter=[master_filter,A]
; if it's not the first filter read in
       IF(nfilter GE 0)THEN BEGIN ;;Don't do for U filter (0)
          this_wave=this_wave[0:npoints-1]
          this_response=this_response[0:npoints-1]
          
           sort_index=sort(this_wave)
           this_response=this_response[sort_index]
           this_wave=this_wave[sort_index]
           
           ;;Negative responses not allowed
           w=WHERE(this_response LT 0.d0,count)
           IF (count GT 0)THEN this_response[w]=0.d0
           
           ;;Multiply in megacam response for MegaCam filters
           IF ~ KEYWORD_SET(noconvolve) THEN BEGIN
              IF (STREGEX(master_filter[nfilter+1].id,'megacam',$
                          /FOLD_CASE,/BOOLEAN)) THEN BEGIN
                 LINTERP,megacam_response_wave,megacam_response,this_wave,$
                         this_megacam_response
                 this_response=this_response*this_megacam_response
                 master_filter[nfilter+1].megacam_included = 1b
                 IF ~noatm THEN $
                    master_filter[nfilter+1].megacam_atm_included = 1b
                 IF(verbose)THEN $
                    PRINT,master_filter[nfilter+1].shortid,$
                          FORMAT='(%"    Filter: %s is a megacam filter, adjusting for system response.")'
              ENDIF
           ENDIF
           
           ;;normalise filter to have a peak throughput of 1
           max_response=MAX(this_response)
           this_response=this_response/max_response
           master_filter[nfilter+1].npoints=npoints
           master_filter[nfilter+1].wave[0:npoints-1]=this_wave
           master_filter[nfilter+1].response[0:npoints-1]=this_response
       ENDIF
      
       nfilter=nfilter+1
       
       nfilterp1 = nfilter+1

       master_filter[nfilterp1].shortid=array[3]
       master_filter[nfilterp1].id=STRTRIM(STRJOIN(array[4:*],' '),2)
       master_filter[nfilterp1].calibtype=array[1]
       master_filter[nfilterp1].responsetype=array[2]

       IF(verbose AND nfilter GT 0)THEN BEGIN
          responsestring='Photons'
          IF(master_filter[nfilter].responsetype EQ 1)THEN responsestring='Energy'
          calibstring='Vega'
          IF(master_filter[nfilter].calibtype EQ 2)THEN calibstring='AB'
          PRINT,nfilter-1,master_filter[nfilter].shortid,master_filter[nfilter].npoints,responsestring,calibstring,$
                 FORMAT='(%"Filter: %3i is %15s (%4i points): %s %s")'
       ENDIF
       npoints=0L
       
       this_response=DBLARR(nmaxpoints)
       this_wave=DBLARR(nmaxpoints)
       micron=0b ;;Reset micron so the next one isn't forced to be micron too
   ENDIF ELSE IF (STREGEX(array[0],'micron',/FOLD_CASE,/BOOLEAN))THEN BEGIN
      micron=1b
   ENDIF ELSE IF (nsplit EQ 2)THEN BEGIN
       ;; keep reading in the current filter
       wave=DOUBLE(array[0])
       response=DOUBLE(array[1])
       
       IF(micron)THEN wave=wave*10000.d0

       IF (master_filter[nfilter+1].responsetype EQ 1) THEN $
         response=response*wave
       
       npoints=npoints+1
       IF(npoints GT nmaxpoints)THEN BEGIN
          PRINT,'ERROR in read_filter_file: Filter '+master_filter[nfilter+1].shortid+' has more data points than allowed ('+STRN(nmaxpoints)+')'
          RETALL
       ENDIF
       this_wave[npoints-1]=wave
       this_response[npoints-1]=response
       
   ENDIF
ENDWHILE
FREE_LUN,unit
; and finish off the last filter read in
this_wave=this_wave[0:npoints-1]
this_response=this_response[0:npoints-1]

sort_index=sort(this_wave)
this_response=this_response[sort_index]
this_wave=this_wave[sort_index]

;;Convolve in MegaCam response
IF ~ KEYWORD_SET(noconvolve) THEN BEGIN
   IF (STREGEX(master_filter[nfilter+1].id,'megacam',/FOLD_CASE,/BOOLEAN)) $
   THEN BEGIN
      LINTERP,megacam_response_wave,megacam_response,this_wave,$
              this_megacam_response
      this_response=this_response*this_megacam_response
      master_filter[nfilter+1].megacam_included = 1b
      IF ~noatm THEN $
         master_filter[nfilter+1].megacam_atm_included = 1b
      IF(verbose)THEN $
         PRINT,master_filter[nfilter+1].shortid,$
                 FORMAT='(%"    Filter: %s is a megacam filter, adjusting for system response.")'
   ENDIF
ENDIF

w=WHERE(this_response LT 0.d0,count)
IF (count GT 0)THEN this_response[w]=0.d0

; normalise filter to have a peak throughput of 1
max_response=MAX(this_response)
this_response=this_response/max_response

master_filter[nfilter+1].npoints=npoints
master_filter[nfilter+1].wave[0:npoints-1]=this_wave
master_filter[nfilter+1].response[0:npoints-1]=this_response

IF(verbose)THEN BEGIN
   responsestring='Photons'
   IF(master_filter[nfilter+1].responsetype EQ 1)THEN responsestring='Energy'
   calibstring='Vega'
   IF(master_filter[nfilter+1].calibtype EQ 2)THEN calibstring='AB'
   PRINT,nfilter,master_filter[nfilter+1].shortid,master_filter[nfilter+1].npoints,responsestring,calibstring,$
         FORMAT='(%"Filter: %3i is %15s (%4i points): %s %s")'
ENDIF


;;Clip of spurious first entry
master_filter=master_filter[1:N_ELEMENTS(master_filter)-1]

number_filters=nfilter+1
IF(verbose) THEN PRINT,number_filters,$
  ' filters read in from',filter_filename

nuniq=N_ELEMENTS(UNIQ(master_filter.shortid,BSORT(master_filter.shortid)))
IF(nuniq NE number_filters)THEN BEGIN
   PRINT,'ERROR in read_filter_file: Some filters have conflicting short_ids. This is not permitted.'
   RETALL
ENDIF

IF ~nosave THEN BEGIN
    IF (verbose) THEN PRINT,"Saving filter info to: ",$
      filter_savefilename
    SAVE,master_filter,FILENAME=filter_savefilename,/VERBOSE
ENDIF

END
