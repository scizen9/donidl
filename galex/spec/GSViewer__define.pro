FUNCTION gsVersion
	return,['v0.4.10 beta','Jun 15, 2012','JDN, TAS']
END

PRO gsCleanUp, tlb

WIDGET_CONTROL, tlb, get_uvalue=self

OBJ_DESTROY, self

END

PRO gsEvents, event

WIDGET_CONTROL, event.top, get_uvalue=self

self->EventHandler, event

END

PRO GSViewer::CleanUp

IF self.autoSave THEN self->SaveTable

WIDGET_CONTROL, self.tlb, /destroy

OBJ_DESTROY, self.spectrum
OBJ_DESTROY, self.stripViewer
OBJ_DESTROY, self.stampViewer
PTR_FREE, self.ptrSpectralFeatures, self.ptrSysX, self.ptrSysY

END

FUNCTION GSViewer::Init, autoSave=autoSave, browser=browser, fuvLimit=fuvLimit, $
  grismPath=grismPath, imagePath=imagePath, inputTable=inputTable, noStamp=noStamp, $
  nuvLimit=nuvLimit, nuvOnly=nuvOnly, radiusLimit=radiusLimit, soda=soda, visit=visit

self.autoSave = KEYWORD_SET( autoSave )
self.noStamp = KEYWORD_SET( noStamp )
self.nuvOnly = KEYWORD_SET( nuvOnly )
self.soda = KEYWORD_SET( soda )

IF N_ELEMENTS( browser ) EQ 0 THEN BEGIN
    IF !VERSION.OS EQ 'darwin' THEN self.browser = 'open'

    IF !VERSION.OS EQ 'linux' THEN self.browser = 'firefox'
ENDIF

spectralFeatures = [ {wv: 912.,  label: 'LL',          psLabel: 'LL' }, $
                     {wv: 937.,  label: 'S VI',        psLabel: 'S VI'  }, $
                     {wv: 973.,  label: 'Ly !4c!3',    psLabel: 'Ly !9g!3' }, $
                     {wv: 991.,  label: 'N III',       psLabel: 'N III' }, $
                     {wv: 1026., label: 'Ly !4b!3',    psLabel: 'Ly !9b!3' }, $
                     {wv: 1034., label: 'O VI',        psLabel: 'O VI' }, $
                     {wv: 1216., label: 'Ly !4a!3',    psLabel: 'Ly !9a!3' }, $
                     {wv: 1260., label: 'Si II',       psLabel: 'Si II' }, $
                     {wv: 1303., label: 'O I + Si II', psLabel: 'O I + Si II' }, $
                     {wv: 1334., label: 'C II',        psLabel: 'C II' }, $
                     {wv: 1397., label: 'Si IV',       psLabel: 'Si IV' }, $
                     {wv: 1467.5, label: 'Ni II',      psLabel: 'Ni II' }, $
                     {wv: 1486., label: 'S I',         psLabel: 'S I' }, $
                     {wv: 1493., label: 'N I',         psLabel: 'N I' }, $
                     {wv: 1526.7, label: 'Si II',      psLabel: 'Si II' }, $
                     {wv: 1533.4, label: 'Si II',      psLabel: 'Si II' }, $
                     {wv: 1549., label: 'C IV',        psLabel: 'C IV' }, $
                     {wv: 1640.5, label: 'He II',      psLabel: 'He II' }, $
                     {wv: 1670.8, label: 'Al II',      psLabel: 'Al II' }, $
                     {wv: 1909., label: 'C III]',      psLabel: 'C III]' }, $
                     {wv: 2800., label: 'Mg II',       psLabel: 'Mg II' } ]

self.ptrSpectralFeatures = PTR_NEW( spectralFeatures )

self.user = GETENV( 'USER' )

topBase = WIDGET_BASE( title='GALEX Spectroscopy Tool', /column, /tlb_kill_request_events, $
                       /tlb_size_events, mbar=menuBar )
self.tlb = topBase

; File menu
fileMenu = WIDGET_BUTTON( menuBar, value='File', /menu )
self.open = WIDGET_BUTTON( fileMenu, value='Open a new tile' )
self.psPlot = WIDGET_BUTTON( fileMenu, value='Create a PS file of the spectrum' )
self.ascii = WIDGET_BUTTON( fileMenu, value='Create an ASCII file of the spectrum' )
self.fits = WIDGET_BUTTON( fileMenu, value='Create a FITS binary table of the spectrum' )
self.shot = WIDGET_BUTTON( fileMenu, value='Create a PS screen shot of the windows' )
self.quit = WIDGET_BUTTON( fileMenu, value='Quit GALEX Spectroscopy tool' )

; Catalog lookup menu
lookupMenu = WIDGET_BUTTON( menuBar, value='Catalog Lookup', /menu )
self.sdss = WIDGET_BUTTON( lookupMenu, value='Look up object in SDSS database' )
self.ned = WIDGET_BUTTON( lookupMenu, value='Look up object in NED database' )

; Help menu
helpMenu = WIDGET_BUTTON( menuBar, value='Help', /help, /menu )
self.about = WIDGET_BUTTON( helpMenu, value='About the GALEX Spectroscopy Tool' )
self.info = WIDGET_BUTTON( helpMenu, value='Keystrokes and Mouse info' )

; Controls
rowBase1 = WIDGET_BASE( topBase, /row )

self.prev = WIDGET_BUTTON( rowBase1, value='Prev' )
self.next = WIDGET_BUTTON( rowBase1, value='Next' )
self.auto = WIDGET_BUTTON( rowBase1, value='Auto Scale' )
self.full = WIDGET_BUTTON( rowBase1, value='Full Scale' )

w = WIDGET_LABEL( rowBase1, value='Smoothing Scale:' )
self.smooth = WIDGET_SLIDER( rowBase1, minimum=1, maximum=25, value=1 )

w = WIDGET_LABEL( rowBase1, value='Spectral Feature:' )
self.feature = WIDGET_COMBOBOX( rowBase1, /dynamic_resize, /editable, $
                                value=STRING( spectralFeatures.wv, format='(i4)' ) )

self.erase = WIDGET_BUTTON( rowBase1, value='Erase Features' )
self.save = WIDGET_BUTTON( rowBase1, value='Save' )

; Big information table
tableLabels = ['ID Spec', 'RA', 'Dec', 'FUV', 'NUV', 'FUV - NUV', 'FUV Quality', $
               'NUV Quality', 'Redshift', 'FUV SNR', 'NUV SNR', 'FUV FWHM', 'NUV FWHM', $
               'Radius', 'GGOID', 'GGOID', 'Notable']
tableWidths = REPLICATE( 75, N_ELEMENTS( tableLabels ) )

self.table = WIDGET_TABLE( topBase, /scroll, /all_events, column_labels=tableLabels, $
                           column_widths=tableWidths, xsize=N_ELEMENTS( tableLabels ) )

; More controls
rowBase2 = WIDGET_BASE( topBase, /row )

coordsBase = WIDGET_BASE( rowBase2, /column, /frame )
w0 = WIDGET_LABEL( coordsBase, value='Coordinates:' )
self.coords = WIDGET_LABEL( coordsBase, /dynamic_resize, value='' )

fluxDensityBase = WIDGET_BASE( rowBase2, /exclusive, /column, /frame )
self.flambda = WIDGET_BUTTON( fluxDensityBase, value='Flam' )
self.fnu = WIDGET_BUTTON( fluxDensityBase, value='Fnu' )
WIDGET_CONTROL, self.flambda, /set_button

toggleBase = WIDGET_BASE( rowBase2, /nonexclusive, /column, /frame )
self.photFlux = WIDGET_BUTTON( toggleBase, value='Overplot photometric flux?' )
self.notable = WIDGET_BUTTON( toggleBase, value='Notable?' )
self.optimal = WIDGET_BUTTON( toggleBase, value='Optimal' )

; Grades
gradeBase = WIDGET_BASE( rowBase2, /column, /frame )

w0 = WIDGET_BASE( gradeBase, /row )
w1 = WIDGET_LABEL( w0, value='NUV Grade:' )
self.nuvGradeBase = WIDGET_BASE( w0, /exclusive, /row )

w0 = WIDGET_BASE( gradeBase, /row )
w1 = WIDGET_LABEL( w0, value='FUV Grade:' )
self.fuvGradeBase = WIDGET_BASE( w0, /exclusive, /row )

self.nuvGrade0 = WIDGET_BUTTON( self.nuvGradeBase, value='Hopeless' )
self.nuvGrade1 = WIDGET_BUTTON( self.nuvGradeBase, value='Bckgrnd Issue(s)' )
self.nuvGrade2 = WIDGET_BUTTON( self.nuvGradeBase, value='No Problems' )

self.fuvGrade0 = WIDGET_BUTTON( self.fuvGradeBase, value='Hopeless' )
self.fuvGrade1 = WIDGET_BUTTON( self.fuvGradeBase, value='Bckgrnd Issue(s)' )
self.fuvGrade2 = WIDGET_BUTTON( self.fuvGradeBase, value='No Problems' )

; Plotting window
self.plotWindow = WIDGET_DRAW( topBase, graphics_level=1, retain=2, xsize=800, ysize=400, $
                               /button_events, /motion_events, /keyboard_events )

; Image strip object
self.stripViewer = OBJ_NEW( 'TwinViewer', /column, group_leader=self.tlb, $
                             panelXSize=1000, panelYSize=150, title='Image Strips' )
self.stripViewer->SetProperty, callbackObj=self, callbackMethod='RedrawImageStrip'

; Postage stamp object
IF NOT self.noStamp THEN BEGIN
    self.stampViewer = OBJ_NEW( 'TwinViewer', /row, group_leader=self.tlb, $
                                panelXSize=256, panelYSize=256, title='Postage Stamps' )
    self.stampViewer->SetProperty, callbackObj=self, callbackMethod='RedrawPostageStamp'
ENDIF

; Collect geometry information for resizing.
geo1 = WIDGET_INFO( self.tlb, /geometry )
geo2 = WIDGET_INFO( self.plotWindow, /geometry )
self.controlXSize = geo1.xsize
self.controlYSize = geo1.ysize - geo2.ysize

WIDGET_CONTROL, topBase, /realize
WIDGET_CONTROL, topBase, set_uvalue=self

WIDGET_CONTROL, self.plotWindow, get_value=id
self.plotWindowID = id

; Display the first spectrum in the table
self->NewTile, fuvLimit=fuvLimit, grismPath=grismPath, imagePath=imagePath, $
               noStamp=noStamp, nuvLimit=nuvLimit, nuvOnly=self.nuvOnly, $
               radiusLimit=radiusLimit, inputTable=inputTable, soda=soda, visit=visit

; Set up the save timer, if necessary
IF self.autoSave THEN WIDGET_CONTROL, self.save, timer=300

XMANAGER, 'GSViewer::Init', self.tlb, event_handler='gsEvents', $
          cleanup='gsCleanUp', /no_block

RETURN, 1

END

PRO GSViewer::Info

message = STRARR( 15 )
message[0] = 'Keystrokes:'
message[1] = ''
message[2] = 'a = auto scale spectrum'
message[3] = 'b = plot background'
message[4] = 'f = full scale spectrum'
message[5] = 'n = next spectrum'
message[6] = 'o = optimal spectrum'
message[7] = 'p = previous spectrum'
message[8] = 'r = redraw spectrum'
message[9] = 'y = spatial profile'
message[10] = ''
message[11] = 'Mouse:'
message[12] = ''
message[13] = 'left button = zoom box'
message[14] = 'right button = mark feature for redshift'

print,message[0],message[1],message[2],message[3],message[4],message[5], $
      message[6],message[7],message[8],message[9],message[10],message[11], $
      message[12], message[13], message[14], $
      format='(%"\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s")'

END

PRO GSViewer::About

ver = gsVersion()

message = STRARR( 18 )
message[0] = 'GALEX Spectroscopy Tool, '+ver[0]
message[1] = ver[2]+' '+ver[1]
message[2] = ' '
message[3] = 'Keystrokes:'
message[4] = ' '
message[5] = 'a = auto scale spectrum'
message[6] = 'b = plot background'
message[7] = 'f = full scale spectrum'
message[8] = 'n = next spectrum'
message[9] = 'o = optimal spectrum'
message[10] = 'p = previous spectrum'
message[11] = 'r = redraw spectrum'
message[12] = 'y = spatial profile'
message[13] = ' '
message[14] = 'Mouse:'
message[15] = ' '
message[16] = 'left button = zoom box'
message[17] = 'right button = mark feature for redshift'

str=STRING(message[0],message[1],message[2],message[3],message[4],message[5], $
       message[6],message[7],message[8],message[9],message[10],message[11],$
       message[12],message[13],message[14],message[15],message[16],message[17],$
       format='(%"%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s \n%s")' )

void = DIALOG_MESSAGE( str, /information, /center )

END

PRO GSViewer::DisplaySpectrum, autoscale=autoscale, limits=limits, subtitle=subtitle, $
  spectrum=spectrum, stamp=stamp, strip=strip, title=title

IF KEYWORD_SET( spectrum ) THEN BEGIN
    fnu = WIDGET_INFO( self.fnu, /button_set )
    WIDGET_CONTROL, self.smooth, get_value=smooth

    IF !D.NAME NE 'PS' THEN WSET, self.plotWindowID
    self.spectrum->PlotSpectrum, autoscale=autoscale, fnu=fnu, limits=limits, $
      subtitle=subtitle, smooth=smooth, title=title, optimal=self.optimalSpectrum

    IF WIDGET_INFO( self.photFlux, /button_set ) THEN self->PlotPhotometricFlux

    self->MarkFeatures

    PTR_FREE, self.ptrSysX, self.ptrSysY
    self.ptrSysX = PTR_NEW( !X )
    self.ptrSysY = PTR_NEW( !Y )
ENDIF

IF KEYWORD_SET( strip ) THEN BEGIN
    self.stripViewer->GetProperty, fuvWindowID=fID, nuvWindowID=nID
       
    WSET, nID
    LOADCT, 0, /silent
    self.spectrum->PlotImageStrip, 'NUV', /annotate, /histEqual

    WSET, fID
    LOADCT, 0, /silent
    self.spectrum->PlotImageStrip, 'FUV', /annotate, /histEqual
ENDIF

IF KEYWORD_SET( stamp ) AND OBJ_VALID( self.stampViewer ) THEN BEGIN
    self.stampViewer->GetProperty, fuvWindowID=fID, nuvWindowID=nID

    WSET, nID
    LOADCT, 0, /silent
    self.spectrum->PlotImage, 'NUV', 60./3600., scalebar=10.

    WSET, fID
    LOADCT, 0, /silent
    self.spectrum->PlotImage, 'FUV', 60./3600., scalebar=10.
ENDIF

END

PRO GSViewer::EraseFeatures

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

row.redshift = !VALUES.F_NAN

WIDGET_CONTROL, self.table, set_value=row, /use_table_select

xr = !X.CRANGE
yr = !Y.CRANGE

self->DisplaySpectrum, limits=[xr[0], yr[0], xr[1], yr[1]], /spectrum

END

PRO GSViewer::EventHandler, ev

origID = !D.WINDOW
WSET, self.plotWindowID

IF PTR_VALID( self.ptrSysX ) THEN !X = *self.ptrSysX
IF PTR_VALID( self.ptrSysY ) THEN !Y = *self.ptrSysY

; Kill request
IF TAG_NAMES( ev, /structure_name ) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    gsCleanUp, ev.top
    RETURN
ENDIF

; Resize request
IF TAG_NAMES( ev, /structure_name ) EQ 'WIDGET_BASE' THEN BEGIN
    WIDGET_CONTROL, self.plotWindow, draw_xsize=(ev.x > self.controlXSize), $
                    draw_ysize=(ev.y - self.controlYSize)
    xr = !X.CRANGE
    yr = !Y.CRANGE
    self->DisplaySpectrum, limits=[xr[0], yr[0], xr[1], yr[1]], /spectrum
    RETURN
ENDIF

CASE ev.id OF 

    self.info: self->Info

    self.about: self->About

    self.ascii: self.spectrum->AsciiFile

    self.auto : self->DisplaySpectrum, /autoscale, /spectrum

    self.erase : self->EraseFeatures

    self.flambda : IF ev.select EQ 1 THEN self->DisplaySpectrum, /autoscale, /spectrum

    self.fits:  BEGIN

        WIDGET_CONTROL, self.table, get_value=row, /use_table_select
        self.spectrum->FITSFile, row

    END

    self.fnu : IF ev.select EQ 1 THEN self->DisplaySpectrum, /autoscale, /spectrum

    self.fuvGrade0 : self->Grade, 'FUV', 0
    self.fuvGrade1 : self->Grade, 'FUV', 1
    self.fuvGrade2 : self->Grade, 'FUV', 2

    self.full : self->DisplaySpectrum, /spectrum

    self.ned : BEGIN

        WIDGET_CONTROL, self.table, get_value=row, /use_table_select
        
        url = 'http://nedwww.ipac.caltech.edu/cgi-bin/nph-objsearch?'
        url += 'search_type=Near+Position+Search&'
        url += 'in_csys=Equatorial&'
        url += 'in_equinox=J2000.0&'
        url += STRING( row.ra, format='(%"lon=%fd&")' )
        url += STRING( row.dec, format='(%"lat=%fd&")' )
        url += 'radius=0.167&'
        url += 'out_csys=Equatorial&'
        url += 'out_equinox=J2000.0&'
        url += 'obj_sort=Distance+to+search+center&'
        url += 'of=pre_text&'
        url += 'zv_breaker=30000.0&'
        url += 'list_limit=5&'
        url += 'img_stamp=YES&'
        url += 'z_constraint=Unconstrained&'
        url += 'z_value1=&z_value2=&z_unit=z&'
        url += 'ot_include=ANY&'
        url += 'nmp_op=ANY'

        SPAWN, [self.browser, url], /noshell

    END

    self.next : self->NextSpectrum

    self.notable : self->Notable, ev.select

    self.nuvGrade0 : self->Grade, 'NUV', 0
    self.nuvGrade1 : self->Grade, 'NUV', 1
    self.nuvGrade2 : self->Grade, 'NUV', 2

    self.open : self->NewTile

    self.plotWindow : self->PlotWindowEvents, ev

    self.photFlux : BEGIN

	IF ev.select EQ 1 THEN BEGIN
	    self->PlotPhotometricFlux
	ENDIF ELSE BEGIN
	    xr = !X.CRANGE
	    yr = !Y.CRANGE
	    self->DisplaySpectrum, limits=[xr[0], yr[0], xr[1], yr[1]], /spectrum
    	ENDELSE

    END

    self.prev : self->PreviousSpectrum

    self.psPlot : self->PostscriptPlot

    self.quit : BEGIN
        
        gsCleanUp, ev.top
        RETURN

    END

    self.save : BEGIN

        self->SaveTable

        IF TAG_NAMES( ev, /structure_name ) EQ 'WIDGET_TIMER' THEN $
          WIDGET_CONTROL, self.save, timer=300

    END

    self.sdss : BEGIN

        WIDGET_CONTROL, self.table, get_value=row, /use_table_select

        url = STRING( row.ra, row.dec, $
                      format='(%"http://cas.sdss.org/dr7/en/tools/explore/obj.asp?ra=%f&dec=%f")' )

        SPAWN, [self.browser, url], /noshell

    END

    self.shot : self->ScreenShot

    self.smooth : BEGIN

	xr = !X.CRANGE
	yr = !Y.CRANGE
	self->DisplaySpectrum, limits=[xr[0], yr[0], xr[1], yr[1]], /spectrum

    END

    self.table : self->TableEvent, ev

    ELSE : 

ENDCASE

WSET, origID

END

PRO GSViewer::Grade, band, value

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

IF band EQ 'FUV' THEN row.fuv_quality = value $
    ELSE row.nuv_quality = value

WIDGET_CONTROL, self.table, set_value=row, /use_table_select

END

PRO GSViewer::MarkFeatures

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

IF NOT FINITE( row.redshift ) THEN RETURN

features = *self.ptrSpectralFeatures

obsLambda = features.wv * (1.+row.redshift)

xr = !X.CRANGE

thick = 1.
labelIndex = 1
IF !D.NAME EQ 'PS' THEN BEGIN
    thick = 2.
    labelIndex = 2
ENDIF

good = WHERE( xr[0] LE obsLambda and obsLambda LE xr[1], nGood )
IF nGood EQ 0 THEN RETURN

FOR i = 0, nGood - 1 DO BEGIN
    index = good[i]
    OPLOT, [obsLambda[index], obsLambda[index]], !Y.CRANGE, $
	color=fsc_color( 'Green' ), thick=thick
    XYOUTS, obsLambda[index], 1.01 * !Y.CRANGE[1], features[index].(labelIndex), $
	orientation=90., color=fsc_color( 'Green' )
ENDFOR

END

PRO GSViewer::NewSpectrum, specID

self.spectrum->NewSpectrum, currentSpecID=specID

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

IF row.fuv_quality EQ -1 THEN BEGIN

    self.spectrum->GetProperty, fuvSigmaSmall=fuvSigmaSmall, fuvSigmaLarge=fuvSigmaLarge

    fuvFluct = fuvSigmaLarge / fuvSigmaSmall

    row.fuv_quality = (fuvFluct GT 1.35) ? 1 : 2

ENDIF

IF row.nuv_quality EQ -1 THEN BEGIN

    self.spectrum->GetProperty, nuvSigmaSmall=nuvSigmaSmall, nuvSigmaLarge=nuvSigmaLarge

    nuvFluct = nuvSigmaLarge / nuvSigmaSmall

    row.nuv_quality = (nuvFluct GT 1.50) ? 1 : 2

ENDIF

WIDGET_CONTROL, self.table, set_value=row, /use_table_select

self->SetGradeButtons

self->SetNotableButton

self->DisplaySpectrum, /autoscale, /spectrum, /strip, /stamp

END

PRO GSViewer::NewTile, fuvLimit=fuvLimit, grismPath=grismPath, imagePath=imagePath, $	
  noStamp=noStamp, nuvLimit=nuvLimit, nuvOnly=nuvOnly, radiusLimit=radiusLimit, $
  inputTable=inputTable, soda=soda, visit=visit

OBJ_DESTROY, self.spectrum

IF KEYWORD_SET( soda ) THEN BEGIN

	IF SIZE( soda, /type ) NE 7 THEN $
		soda = DIALOG_PICKFILE( /directory, /must_exist, $
					title='Please select the top tile directory' )

        IF KEYWORD_SET( visit ) THEN BEGIN
            self.grismPath = soda + '/g/00-visits/' + STRING( visit, format='(i4.4)' ) + $
                             '-img/use/'
        ENDIF ELSE self.grismPath = soda + '/g/01-main/use/use/'
                             
	self.imagePath = soda + '/d/01-main/use/use/'
	
ENDIF ELSE BEGIN

	IF N_ELEMENTS( grismPath ) EQ 0 THEN $
		self.grismPath = DIALOG_PICKFILE( /directory, /must_exist, $
                                                  title='Please select the grism directory' ) $
          ELSE self.grismPath = grismPath

        IF NOT self.noStamp THEN BEGIN
            IF N_ELEMENTS( imagePath ) EQ 0 THEN $
              self.imagePath = DIALOG_PICKFILE( /directory, /must_exist, $
                                                title='Please select the image directory' ) $
            ELSE self.imagePath = imagePath
        ENDIF
										  
ENDELSE

WIDGET_CONTROL, /hourglass

self.spectrum = OBJ_NEW( 'GALEXSpectrum', self.grismPath, self.imagePath, $
                         noStamp=noStamp, nuvOnly=nuvOnly, visit=visit )
IF NOT OBJ_VALID( self.spectrum ) THEN RETURN

self.spectrum->GetProperty, tile=tile
WIDGET_CONTROL, self.tlb, tlb_set_title=tile

IF N_ELEMENTS( inputTable ) EQ 0 THEN BEGIN 
    table = self.spectrum->GSViewerTable( fuvLimit=fuvLimit, nuvLimit=nuvLimit, $
                                          radiusLimit=radiusLimit )
ENDIF ELSE table = MRDFITS( inputTable, 1, /silent, /unsigned )

format = ['(i6)', '(f10.6)', '(f10.6)', '(f5.2)', '(f5.2)', '(f5.2)', '(i2)', '(i2)', $
          '(f6.3)', '(f6.1)', '(f6.1)', '(f5.1)', '(f5.1)', '(f5.3)', '(i10)', '(i10)', '(i3)']
self.tableXSize = N_ELEMENTS( format )
self.tableYSize = N_ELEMENTS( table )
tableFormat = STRARR( self.tableXSize, self.tableYSize )
FOR i = 0, self.tableYSize - 1 DO tableFormat[*, i] = format

WIDGET_CONTROL, self.table, table_xsize=self.tableXSize
WIDGET_CONTROL, self.table, table_ysize=self.tableYSize
WIDGET_CONTROL, self.table, format=tableFormat
WIDGET_CONTROL, self.table, set_value=table
WIDGET_CONTROL, self.table, set_table_select=[0, 0, self.tableXSize - 1, 0]

self->NewSpectrum, table[0].id

END

PRO GSViewer::NextSpectrum

next = WIDGET_INFO( self.table, /table_select )
view = WIDGET_INFO( self.table, /table_view )

next[0] = 0
next[1] = (self.tableYSize - 1) < ++next[1]
next[2] = self.tableXSize - 1
next[3] = (self.tableYSize - 1) < ++next[3]

WIDGET_CONTROL, self.table, set_table_select=next
WIDGET_CONTROL, self.table, set_table_view=[view[0], next[1]-1]

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

self->NewSpectrum, row.id

END

PRO GSViewer::Notable, value

WIDGET_CONTROL, self.table, get_value=row, /use_table_select
row.notable = value
WIDGET_CONTROL, self.table, set_value=row, /use_table_select

WIDGET_CONTROL, self.notable, set_button=value

END

PRO GSViewer::PlotPhotometricFlux

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

IF FINITE( row.fuv ) THEN BEGIN
    fnu = 10^(-0.4*(row.fuv + 48.6))

    IF WIDGET_INFO( self.fnu, /button_set ) THEN val = fnu $
	ELSE val = fnu * 3.e18 / 1525.^2

    OPLOT, [1350., 1800.], [val, val], color=fsc_color( 'Blue' )
ENDIF


IF FINITE( row.nuv ) THEN BEGIN
    fnu = 10^(-0.4*(row.nuv + 48.6))

    IF WIDGET_INFO( self.fnu, /button_set ) THEN val = fnu $
	ELSE val = fnu * 3.e18 / 2300.^2

    OPLOT, [1850., 3000.], [val, val], color=fsc_color( 'Blue' )
ENDIF

END

PRO GSViewer::PlotWindowEvents, ev

coords = CONVERT_COORD( ev.x, ev.y, /device, /to_data )

WIDGET_CONTROL, self.coords, $
                set_value=STRING( coords[0], coords[1], format='(f6.1, 1x, e9.2)' )

WIDGET_CONTROL, self.plotWindow, /input_focus

CASE ev.type OF 

    0: BEGIN ; Button press

        IF ev.press EQ 1 THEN BEGIN ; Left button

            self.drawingZoomBox = 1B

            geo = WIDGET_INFO( self.plotWindow, /geometry )
            WINDOW, /free, /pixmap, xsize=geo.draw_xsize, ysize=geo.draw_ysize
            self.pixMapID = !D.WINDOW
            DEVICE, copy=[0, 0, geo.draw_xsize, geo.draw_ysize, 0, 0, self.plotWindowID]

            self.zoomBoxX = coords[0]
            self.zoomBoxY = coords[1]

        ENDIF

	IF ev.press EQ 4 THEN self->Redshift, coords[0]

    END

    1: BEGIN ; Button release

        IF ev.release EQ 1 THEN BEGIN ; Left button
            
            self.drawingZoomBox = 0B

            geo = WIDGET_INFO( self.plotWindow, /geometry )
            DEVICE, copy=[0, 0, geo.draw_xsize, geo.draw_ysize, 0, 0, self.pixMapID]
            WDELETE, self.pixMapID

            x0 = MIN( [self.zoomBoxX, coords[0]], max=x1 )
            y0 = MIN( [self.zoomBoxY, coords[1]], max=y1 )

	    self->DisplaySpectrum, limits=[x0, y0, x1, y1], /spectrum

        ENDIF

    END

    2: BEGIN ; Mouse motion

        IF self.drawingZoomBox THEN BEGIN

            geo = WIDGET_INFO( self.plotWindow, /geometry )
            DEVICE, copy=[0, 0, geo.draw_xsize, geo.draw_ysize, 0, 0, self.pixMapID]

            PLOTS, [self.zoomBoxX, coords[0], coords[0], self.zoomBoxX, self.zoomBoxX], $
                   [self.zoomBoxY, self.zoomBoxY, coords[1], coords[1], self.zoomBoxY], $
                   color=fsc_color( 'Red' )

        ENDIF

    END

    5: BEGIN ; Key press

        IF ev.press NE 1B THEN BREAK

        CASE ev.ch OF

            48: self->Notable, 0 ; "0" for not notable

            49: self->Notable, 1 ; "1" for star

            50: self->Notable, 2 ; "2" for galaxy

            51: self->Notable, 3 ; "3" for Lyman-alpha galaxy

            52: self->Notable, 4 ; "4" for quasar

            97: self->DisplaySpectrum, /autoscale, /spectrum ; "a" for autoscale

            98: self.spectrum->PlotBackground ; "b" for background

            102: self->DisplaySpectrum, /spectrum ; "f" for full scale

            110: self->NextSpectrum ; "n" for next

            111: BEGIN ; "o" for optimal spectrum

                self.optimalSpectrum = self.optimalSpectrum XOR 1B
                WIDGET_CONTROL, self.optimal, set_button=self.optimalSpectrum
                self->DisplaySpectrum, /autoscale, /spectrum

            END

            112: self->PreviousSpectrum ; "p" for previous

            114: BEGIN ; "r" for redraw

                xr = !X.CRANGE
                yr = !Y.CRANGE

                self->DisplaySpectrum, limits=[xr[0], yr[0], xr[1], yr[1]], /spectrum

            END

            121: self.spectrum->PlotProfile ; "y" for y profile

            ELSE:

        ENDCASE

    END

    ELSE: 
    
ENDCASE

END

PRO GSViewer::PostscriptPlot

xr = !X.CRANGE
yr = !Y.CRANGE

self.spectrum->GetProperty, currentSpecID=id, tile=tile
self.spectrum->GetProperty, currentSpecID=id, obsdate=obsdate
self.spectrum->GetProperty, currentSpecID=id, eclipse=eclipse
self.spectrum->GetProperty, currentSpecID=id, visit=visit
self.spectrum->GetProperty, currentSpecID=id, subvis=subvis
self.spectrum->GetProperty, currentSpecID=id, exposure=exposure

fileName = STRING( tile, id, format='(%"%s_%d.ps")' )

ver = gsVersion()

SET_PLOT, 'PS'
!P.FONT = 0
DEVICE, /color, /landscape, file=fileName, /helvetica, /isolatin1

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

title = STRING( tile, id, row.ra, row.dec, row.fuv, row.nuv, row.redshift, $
	        format='(%"%s %d %10.6f %10.6f FUV = %5.2f NUV = %5.2f z = %6.3f")' )

self->DisplaySpectrum, limits=[xr[0], yr[0], xr[1], yr[1]], /spectrum, title=title

; Add username and time.
XYOUTS, 0.995, 0.079, STRING( self.user, SYSTIME(), ver[0], format='(%"%s %s %s")' ), $
	/normal, orientation=90.

; Add identifying data
XYOUTS, 0.35, 0.90, STRING( obsdate, eclipse, visit, subvis, exposure, $
	format='(%"%s  Ecl:%d  Vis:%d  Svs:%d  Exp:%10.1f")' ), /normal

DEVICE, /close

SET_PLOT, 'X'
!P.FONT = -1

END

PRO GSViewer::PreviousSpectrum

prev = WIDGET_INFO( self.table, /table_select )
view = WIDGET_INFO( self.table, /table_view )

prev[0] = 0
prev[1] = 0 > --prev[1]
prev[2] = self.tableXSize - 1
prev[3] = 0 > --prev[3]

WIDGET_CONTROL, self.table, set_table_select=prev
WIDGET_CONTROL, self.table, set_table_view=[view[0], prev[1]-1]

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

self->NewSpectrum, row.id

END

PRO GSViewer::RedrawImageStrip, band, histEqual=histEqual, limits=limits

IF band EQ 'FUV' THEN BEGIN
    self.stripViewer->GetProperty, fuvWindowID=fID
    WSET, fID
ENDIF ELSE BEGIN
    self.stripViewer->GetProperty, nuvWindowID=nID
    WSET, nID
ENDELSE
        
LOADCT, 0, /silent
self.spectrum->PlotImageStrip, band, /annotate, histEqual=histEqual, limits=limits

END

PRO GSViewer::RedrawPostageStamp, band, histEqual=histEqual, limits=limits

IF NOT OBJ_VALID( self.stampViewer ) THEN RETURN

IF band EQ 'FUV' THEN BEGIN
    self.stampViewer->GetProperty, fuvWindowID=fID
    WSET, fID
ENDIF ELSE BEGIN
    self.stampViewer->GetProperty, nuvWindowID=nID
    WSET, nID
ENDELSE
        
LOADCT, 0, /silent
self.spectrum->PlotImage, band, 60./3600., histEqual=histEqual, limits=limits, scalebar=10.

END

PRO GSViewer::Redshift, obsLambda

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

value = WIDGET_INFO( self.feature, /combobox_gettext )
restLambda = FLOAT( value )

row.redshift = (obsLambda / restLambda) - 1.

WIDGET_CONTROL, self.table, set_value=row, /use_table_select

xr = !X.CRANGE
yr = !Y.CRANGE

self->DisplaySpectrum, limits=[xr[0], yr[0], xr[1], yr[1]], /spectrum

END

PRO GSViewer::SaveTable

WIDGET_CONTROL, self.table, get_value=table

self.spectrum->GetProperty, tile=tile

hdr = STRARR( 4 )
hdr[0] = STRING( self.user, format='(%"USER    = ''%s''")' )
hdr[1] = STRING( SYSTIME(), format='(%"TIMESTMP= ''%s''")' )
hdr[2] = STRING( tile, format='(%"TILE    = ''%s''")' )

MWRFITS, table, tile + '-qa.fits', hdr, /create

END

PRO GSViewer::ScreenShot

self.spectrum->GetProperty, currentSpecID=id, tile=tile
self.spectrum->GetProperty, currentSpecID=id, obsdate=obsdate
self.spectrum->GetProperty, currentSpecID=id, eclipse=eclipse
self.spectrum->GetProperty, currentSpecID=id, visit=visit
self.spectrum->GetProperty, currentSpecID=id, subvis=subvis
self.spectrum->GetProperty, currentSpecID=id, exposure=exposure

fileName = STRING( tile, id, format='(%"%s_%d_screen_shot.ps")' )

SET_PLOT, 'ps'
!P.FONT = 0
DEVICE, /color, bits=8, xoffset=0., yoffset=0., xsize=14.67, ysize=11.33, /inches, $
        file=fileName, /helvetica, /isolatin1

WIDGET_CONTROL, self.table, get_value=row, /use_table_select
infoString = STRING( tile, id, format='(%"%s %d ")' )
infoString += STRING( row.ra, row.dec, row.fuv, row.nuv, $
                      format='(%"RA = %10.6f Dec = %10.6f FUV = %5.2f NUV = %5.2f ")' )
infoString += STRING( row.fuv_quality, row.nuv_quality, row.redshift, row.radius, $
                      format='(%"FUV Quality = %2d NUV Quality = %2d z = %6.3f radius = %5.3f" )' )

ver = gsVersion()
verString = STRING( self.user, SYSTIME(), ver[0], format='(%"%s %s %s")' )

self.spectrum->PlotSummary, infoString=infoString, verString=verString

DEVICE, /close

SET_PLOT, 'x'
!P.FONT = -1

END

PRO GSViewer::SetGradeButtons

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

CASE row.fuv_quality OF

    -1: WIDGET_CONTROL, self.fuvGradeBase, set_button=0
     0: WIDGET_CONTROL, self.fuvGrade0, set_button=1
     1: WIDGET_CONTROL, self.fuvGrade1, set_button=1
     2: WIDGET_CONTROL, self.fuvGrade2, set_button=1

     ELSE:

ENDCASE

CASE row.nuv_quality OF

    -1: WIDGET_CONTROL, self.nuvGradeBase, set_button=0
     0: WIDGET_CONTROL, self.nuvGrade0, set_button=1
     1: WIDGET_CONTROL, self.nuvGrade1, set_button=1
     2: WIDGET_CONTROL, self.nuvGrade2, set_button=1

     ELSE:

ENDCASE

END

PRO GSViewer::SetNotableButton

WIDGET_CONTROL, self.table, get_value=row, /use_table_select

IF row.notable THEN WIDGET_CONTROL, self.notable, set_button=1 $
  ELSE WIDGET_CONTROL, self.notable, set_button=0

END

PRO GSViewer::SetProperty, autoSave=autoSave

IF N_ELEMENTS( autoSave ) NE 0 THEN self.autoSave = KEYWORD_SET( autoSave )

END

PRO GSViewer::TableEvent, ev

; Ignore all events except table selection events.
IF ev.type NE 4 THEN RETURN

; Ignore table deselection events.
IF ev.sel_left EQ -1 THEN RETURN

; A new spectrum.
IF ev.sel_left EQ 0 AND ev.sel_right EQ (self.tableXSize - 1) THEN BEGIN
    WIDGET_CONTROL, self.table, get_value=row, /use_table_select
    self->NewSpectrum, row.id
ENDIF

; Sort the table.
IF ev.sel_top EQ 0 AND ev.sel_bottom EQ (self.tableYSize - 1) THEN BEGIN
    WIDGET_CONTROL, self.table, get_value=table, /no_copy
    view = WIDGET_INFO( self.table, /table_view )

    self.spectrum->GetProperty, currentSpecID=id

    index = ev.sel_left
    sort = SORT( table.(index) )
    table = TEMPORARY( table[sort] )
    WIDGET_CONTROL, self.table, set_value=table, /no_copy

    row = WHERE( id EQ table.id )
    WIDGET_CONTROL, self.table, set_table_select=[0, row, self.tableXSize - 1, row]
    WIDGET_CONTROL, self.table, set_table_view=[view[0], row - 1]
ENDIF

END

PRO GSViewer__define

define = { GSViewer, $
           autoSave: 0B, $
           browser: '', $
           grismPath: '', $
           imagePath: '', $
           soda: 0B, $
           noStamp: 0B, $
           NUVonly: 0B, $
           spectrum: OBJ_NEW(), $
           stripViewer: OBJ_NEW(), $
           stampViewer: OBJ_NEW(), $
           user: '', $
           ptrSpectralFeatures: PTR_NEW(), $
           tlb: 0L, $
           open: 0L, $
           psPlot: 0L, $
           ascii: 0L, $
           fits: 0L, $
           shot: 0L, $
           quit: 0L, $
           sdss: 0L, $
           ned: 0L, $
           info: 0L, $
           about: 0L, $
           prev: 0L, $
           next: 0L, $
           auto: 0L, $
           full: 0L, $
           smooth: 0L, $
           feature: 0L, $
           erase: 0L, $
           save: 0L, $
           table: 0L, $
           tableXSize: 0, $
           tableYSize: 0, $
           coords: 0L, $
           flambda: 0L, $
           fnu: 0L, $
           photFlux: 0L, $
           notable: 0L, $
           optimal: 0L, $
           nuvGradeBase: 0L, $
           nuvGrade0: 0L, $
           nuvGrade1: 0L, $
           nuvGrade2: 0L, $
           fuvGradeBase: 0L, $
           fuvGrade0: 0L, $
           fuvGrade1: 0L, $
           fuvGrade2: 0L, $
           plotWindow: 0L, $
           plotWindowID: 0L, $
           controlXSize: 0, $
           controlYSize: 0, $
           ptrSysX: PTR_NEW(), $
           ptrSysY: PTR_NEW(), $
           pixMapID: 0L, $
           drawingZoomBox: 0B, $
           optimalSpectrum: 0B, $
           zoomBoxX: 0., $
           zoomBoxY: 0. }

END

PRO RunGALEXSpec, autoSave=autoSave, browser=browser, fuvLimit=fuvLimit, $
                  grismPath=grismPath, imagePath=imagePath, inputTable=inputTable, $
                  noStamp=noStamp, nuvLimit=nuvLimit, nuvOnly=nuvOnly, $
                  radiusLimit=radiusLimit, soda=soda, visit=visit, object=object

object = OBJ_NEW( 'GSViewer', autoSave=autoSave, browser=browser, fuvLimit=fuvLimit, $
                  grismPath=grismPath, imagePath=imagePath, inputTable=inputTable, $
                  noStamp=noStamp, nuvLimit=nuvLimit, nuvOnly=nuvOnly, $
                  radiusLimit=radiusLimit, soda=soda, visit=visit )

END
