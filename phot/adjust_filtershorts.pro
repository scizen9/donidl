;+
;NAME
; adjust_filtershorts
;PURPOSE
; To handle the tiring business of appending the right extension
; to filter short titles depending on the type of Landolt or CFHT
; filters
;USAGE
; newshorts = adjust_filtershorts( shorts, [LANDOLTFILTTYPE=landoltfilttype,$
;                                  CFHTFILTTYPE=cfhtfilttype )
;INPUTS
;  shorts List of filter names (i.e., ['B','V','gs']).
;OPTIONAL INPUTS
;  cfhtfilttype      Type of megacam filter scan. Should be a string equal to
;                     either 'sagem', 'cfhtnew' or 'cfhtold'. The default is
;                     'none', which means leave the filters alone
;  landoltfilttype   Which landolt filters to use.  Options are:
;                     'bessell' 'cohen', 'stritzinger',
;                      'shift' and 'astier'.  The default is 'none'
;                      which means leave the filters alone.
;RETURNS
; The filter names with the appropriate thing appended
;MODIFICATION HISTORY
; Author: Alex Conley, Sep 2006
;-

;;Damn, what a boring function

FUNCTION adjust_landoltfiltershorts, origfilt, landoltfilttype

    CASE landoltfilttype OF
        'none' : BEGIN
            newname = origfilt
        END
        'bessell' : BEGIN
            SWITCH origfilt OF 
                'U-cohen' :
                'U-stritzinger' :
                'U-maiz' : BEGIN
                    newname = 'U'
                    BREAK
                END
                'B-cohen' : 
                'B-shift' :
                'B-stritzinger' :
                'B-astier' : BEGIN
                    newname = 'B'
                    BREAK
                END
                'V-cohen' : 
                'V-shift' :
                'V-stritzinger' :
                'V-astier' : BEGIN
                    newname = 'V'
                    BREAK
                END
                'R-cohen' : 
                'R-shift' :
                'R-stritzinger' :
                'R-astier' : BEGIN
                    newname = 'R'
                    BREAK
                END
                'I-cohen' : 
                'I-shift' :
                'I-stritzinger' :
                'I-astier' : BEGIN
                    newname = 'I'
                    BREAK
                END
                ELSE : newname = origfilt
            END
        END
        'cohen' : BEGIN
            SWITCH origfilt OF 
                'U' :
                'U-stritzinger' :
                'U-maiz' : BEGIN
                    newname = 'U-cohen'
                    BREAK
                END
                'B' : 
                'B-shift' :
                'B-stritzinger' :
                'B-astier' : BEGIN
                    newname = 'B-cohen'
                    BREAK
                END
                'V' : 
                'V-shift' :
                'V-stritzinger' :
                'V-astier' : BEGIN
                    newname = 'V-cohen'
                    BREAK
                END
                'R' : 
                'R-shift' :
                'R-stritzinger' :
                'R-astier' : BEGIN
                    newname = 'R-cohen'
                    BREAK
                END
                'I' : 
                'I-shift' :
                'I-stritzinger' :
                'I-astier' : BEGIN
                    newname = 'I-cohen'
                    BREAK
                END
                ELSE : newname = origfilt
            END
        END
        'astier' : BEGIN
            SWITCH origfilt OF 
                'U-cohen' :
                'U-stritzinger' :
                'U-maiz' : BEGIN
                    newname = 'U'
                    BREAK
                END
                'B' : 
                'B-shift' :
                'B-stritzinger' :
                'B-cohen' : BEGIN
                    newname = 'B-astier'
                    BREAK
                END
                'V' : 
                'V-shift' :
                'V-stritzinger' :
                'V-cohen' : BEGIN
                    newname = 'V-astier'
                    BREAK
                END
                'R' : 
                'R-shift' :
                'R-stritzinger' :
                'R-cohen' : BEGIN
                    newname = 'R-astier'
                    BREAK
                END
                'I' : 
                'I-shift' :
                'I-stritzinger' :
                'I-cohen' : BEGIN
                    newname = 'I-astier'
                    BREAK
                END
                ELSE : newname = origfilt
            END
        END
        'shift' : BEGIN
            SWITCH origfilt OF 
                'U-cohen' :
                'U-stritzinger' :
                'U-maiz' : BEGIN
                    newname = 'U'
                    BREAK
                END
                'B' : 
                'B-astier' :
                'B-stritzinger' :
                'B-cohen' : BEGIN
                    newname = 'B-shift'
                    BREAK
                END
                'V' : 
                'V-astier' :
                'V-stritzinger' :
                'V-cohen' : BEGIN
                    newname = 'V-shift'
                    BREAK
                END
                'R' : 
                'R-astier' :
                'R-stritzinger' :
                'R-cohen' : BEGIN
                    newname = 'R-shift'
                    BREAK
                END
                'I' : 
                'I-astier' :
                'I-stritzinger' :
                'I-cohen' : BEGIN
                    newname = 'I-shift'
                    BREAK
                END
                ELSE : newname = origfilt
            END
        END
        'stritzinger' : BEGIN
            SWITCH origfilt OF 
                'U' :
                'U-cohen' :
                'U-maiz' : BEGIN
                    newname = 'U-stritzinger'
                    BREAK
                END
                'B' : 
                'B-astier' :
                'B-shift' :
                'B-cohen' : BEGIN
                    newname = 'B-stritzinger'
                    BREAK
                END
                'V' : 
                'V-astier' :
                'V-shift' :
                'V-cohen' : BEGIN
                    newname = 'V-stritzinger'
                    BREAK
                END
                'R' : 
                'R-astier' :
                'R-shift' :
                'R-cohen' : BEGIN
                    newname = 'R-stritzinger'
                    BREAK
                END
                'I' : 
                'I-astier' :
                'I-shift' :
                'I-cohen' : BEGIN
                    newname = 'I-stritzinger'
                    BREAK
                END
                ELSE : newname = origfilt
            END
        END
        ELSE : MESSAGE,"ERROR in adjust_filtershorts: unknown landolt type "+$
                       landoltfilttype
    END
RETURN,newname
END

FUNCTION adjust_cfhtfiltershorts, origfilt, cfhtfilttype

    CASE cfhtfilttype OF
        'none' : BEGIN
            newname = origfilt
        END
        'cfhtold' : BEGIN
            SWITCH origfilt OF 
                'usSAGEM' :
                'usCFHT' : BEGIN
                    newname = 'us'
                    BREAK
                END
                'gsSAGEM' :
                'gsCFHT' : BEGIN
                    newname = 'gs'
                    BREAK
                END
                'rsSAGEM' :
                'rsCFHT' : BEGIN
                    newname = 'rs'
                    BREAK
                END
                'isSAGEM' :
                'isCFHT' : BEGIN
                    newname = 'is'
                    BREAK
                END
                'zsSAGEM' :
                'zsCFHT' : BEGIN
                    newname = 'zs'
                    BREAK
                END
                ELSE : newname = origfilt
            END
        END
        'cfhtnew' : BEGIN
            SWITCH origfilt OF 
                'usSAGEM' :
                'us' : BEGIN
                    ;;Don't have new u' scan
                    newname = 'usSAGEM'
                    BREAK
                END
                'gsSAGEM' :
                'gs' : BEGIN
                    newname = 'gsCFHT'
                    BREAK
                END
                'rsSAGEM' :
                'rs' : BEGIN
                    newname = 'rsCFHT'
                    BREAK
                END
                'isSAGEM' :
                'is' : BEGIN
                    newname = 'isCFHT'
                    BREAK
                END
                'zsSAGEM' :
                'zs' : BEGIN
                    ;;Don't have new filter scans for z'
                    newname = 'zsSAGEM'
                    BREAK
                END
                ELSE : newname = origfilt
            END
        END
        'sagem' : BEGIN
            SWITCH origfilt OF 
                'usCFHT' :
                'us' : BEGIN
                    newname = 'usSAGEM'
                    BREAK
                END
                'gsCFHT' :
                'gs' : BEGIN
                    newname = 'gsSAGEM'
                    BREAK
                END
                'rsCFHT' :
                'rs' : BEGIN
                    newname = 'rsSAGEM'
                    BREAK
                END
                'isCFHT' :
                'is' : BEGIN
                    newname = 'isSAGEM'
                    BREAK
                END
                'zsCFHT' :
                'zs' : BEGIN
                    newname = 'zsSAGEM'
                    BREAK
                END
                ELSE : newname = origfilt
            END
        END
        ELSE : MESSAGE,"ERROR in adjust_filtershorts: unknown cfht type "+$
                       cfhtfilttype
    END
RETURN,newname
END


FUNCTION adjust_filtershorts, shorts, LANDOLTFILTTYPE=landoltfilttype,$
  CFHTFILTTYPE=cfhtfilttype

IF N_ELEMENTS( shorts ) EQ 0 THEN RETURN, ''

IF N_ELEMENTS( landoltfilttype ) EQ 0 THEN landoltfilttype = 'none'
IF N_ELEMENTS( cfhtfilttype ) EQ 0 THEN cfhtfilttype = 'none'

IF landoltfilttype EQ 'none' AND cfhtfilttype EQ 'none' THEN $
  RETURN,shorts

newshorts = shorts
;;All of the landolt filters start with one of these characters
landoltfirst = ['U','B','V','R','I']
;;And all of the CFHT filter names with one of these
cfhtfirsttwo = ['us','gs','rs','is','zs']
FOR i = 0, N_ELEMENTS( shorts ) - 1 DO BEGIN
    origfilt = shorts[i]
    firstchar = STRMID( origfilt, 0, 1 )
    wl = WHERE( firstchar EQ landoltfirst, countwl )
    IF landoltfilttype NE 'none' AND countwl NE 0 THEN BEGIN
        newname = adjust_landoltfiltershorts( origfilt, landoltfilttype )
    ENDIF ELSE BEGIN
        secondchar = STRMID( origfilt, 0, 2 )
        wc = WHERE( secondchar EQ cfhtfirsttwo, countwc )
        IF cfhtfilttype NE 'none' AND countwc NE 0 THEN BEGIN
            newname = adjust_cfhtfiltershorts( origfilt, cfhtfilttype )
        ENDIF ELSE newname = origfilt
    ENDELSE
    newshorts[i] = newname
ENDFOR

RETURN,newshorts

END
