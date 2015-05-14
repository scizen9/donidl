PRO tvCleanUp, tlb

WIDGET_CONTROL, tlb, get_uvalue=self

OBJ_DESTROY, self

END

PRO tvEvents, event

WIDGET_CONTROL, event.top, get_uvalue=self

self->EventHandler, event

END

PRO TwinViewer::CleanUp

WIDGET_CONTROL, self.tlb, /destroy

END

FUNCTION TwinViewer::Init, column=column, group_leader=group_leader, $
  panelXSize=panelXSize, panelYSize=panelYSize, row=row, title=title

self.panelXSize = 256
self.panelYSize = 256
self.title = 'GALEX'

IF KEYWORD_SET( column ) THEN self.column = 1B
IF KEYWORD_SET( row ) THEN self.row = 1B
IF N_ELEMENTS( panelXSize ) NE 0 THEN self.panelXSize = panelXSize
IF N_ELEMENTS( panelYSize ) NE 0 THEN self.panelYSize = panelYSize
IF N_ELEMENTS( title ) NE 0 THEN self.title = title

IF N_ELEMENTS( group_leader ) EQ 0 THEN BEGIN
    self.tlb = WIDGET_BASE( column=KEYWORD_SET( column ), row=KEYWORD_SET( row ), $
                            title=self.title, /tlb_size_events )
ENDIF ELSE BEGIN
    self.tlb = WIDGET_BASE( column=KEYWORD_SET( column ), row=KEYWORD_SET( row ), $
                            title=self.title, group_leader=group_leader, /tlb_size_events )
ENDELSE

self.nuvWindow = WIDGET_DRAW( self.tlb, graphics_level=1, retain=2, $
                              xsize=self.panelXSize, ysize=self.panelYSize, $
                              /button_events, /motion_events, /keyboard_events )

self.fuvWindow = WIDGET_DRAW( self.tlb, graphics_level=1, retain=2, $
                              xsize=self.panelXSize, ysize=self.panelYSize, $
                              /button_events, /motion_events, /keyboard_events )

WIDGET_CONTROL, self.tlb, /realize
WIDGET_CONTROL, self.tlb, set_uvalue=self

WIDGET_CONTROL, self.nuvWindow, get_value=id
self.nuvWindowID = id
WIDGET_CONTROL, self.fuvWindow, get_value=id
self.fuvWindowID = id

XMANAGER, 'TwinViewer::Init', self.tlb, event_handler='tvEvents', $
          cleanup='tvCleanUp', /no_block

RETURN, 1

END

PRO TwinViewer::EventHandler, ev

; Resize event
IF TAG_NAMES( ev, /structure_name ) EQ 'WIDGET_BASE' THEN BEGIN

    IF self.column THEN BEGIN
        WIDGET_CONTROL, self.fuvWindow, draw_xsize=ev.x, draw_ysize=ev.y/2
        WIDGET_CONTROL, self.nuvWindow, draw_xsize=ev.x, draw_ysize=ev.y/2
    ENDIF ELSE BEGIN
        WIDGET_CONTROL, self.fuvWindow, draw_xsize=ev.x/2, draw_ysize=ev.y
        WIDGET_CONTROL, self.nuvWindow, draw_xsize=ev.x/2, draw_ysize=ev.y
    ENDELSE

    IF OBJ_VALID( self.callbackObj ) THEN BEGIN
        CALL_METHOD, self.callbackMethod, self.callbackObj, 'FUV'
        CALL_METHOD, self.callbackMethod, self.callbackObj, 'NUV'
    ENDIF

    RETURN
    
ENDIF

CASE ev.id OF

    self.fuvWindow : BEGIN

        IF ev.type EQ 0 THEN self.stretching = 1B
        IF ev.type EQ 1 THEN self.stretching = 0B

        IF self.stretching THEN BEGIN
            coords = CONVERT_COORD( [ev.x, ev.y, 0], /device, /to_normal )
            self.fuvStretch = [coords[0], coords[1]]

            IF OBJ_VALID( self.callbackObj ) THEN $
              CALL_METHOD, self.callbackMethod, self.callbackObj, 'FUV', $
                           limits=self.fuvStretch

        ENDIF

        IF ev.type EQ 5 AND ev.ch EQ 104 AND ev.press EQ 1B THEN $
          CALL_METHOD, self.callbackMethod, self.callbackObj, 'FUV', /histEqual

    END

    self.nuvWindow : BEGIN

        IF ev.type EQ 0 THEN self.stretching = 1B
        IF ev.type EQ 1 THEN self.stretching = 0B

        IF self.stretching THEN BEGIN
            coords = CONVERT_COORD( [ev.x, ev.y, 0], /device, /to_normal )
            self.nuvStretch = [coords[0], coords[1]]

            IF OBJ_VALID( self.callbackObj ) THEN $
              CALL_METHOD, self.callbackMethod, self.callbackObj, 'NUV', $
                           limits=self.nuvStretch

        ENDIF

        IF ev.type EQ 5 AND ev.ch EQ 104 AND ev.press EQ 1B THEN $
          CALL_METHOD, self.callbackMethod, self.callbackObj, 'NUV', /histEqual

    END

    ELSE : BEGIN

        ; Do nothing

    END

ENDCASE

END

PRO TwinViewer::GetProperty, fuvWindowID=fuvWindowID, nuvWindowID=nuvWindowID, $
  fuvStretch=fuvStretch, nuvStretch=nuvStretch, fuvHistEqual=fuvHistEqual, $
  nuvHistEqual=nuvHistEqual

IF ARG_PRESENT( fuvWindowID ) THEN fuvWindowID = self.fuvWindowID

IF ARG_PRESENT( nuvWindowID ) THEN nuvWindowID = self.nuvWindowID

IF ARG_PRESENT( fuvStretch ) THEN fuvStretch = self.fuvStretch

IF ARG_PRESENT( nuvStretch ) THEN nuvStretch = self.nuvStretch

IF ARG_PRESENT( fuvHistEqual ) THEN fuvHistEqual = self.fuvHistEqual

IF ARG_PRESENT( nuvHistEqual ) THEN nuvHistEqual = self.nuvHistEqual

END

PRO TwinViewer::SetProperty, callbackObj=callbackObj, callbackMethod=callbackMethod

IF N_ELEMENTS( callbackObj ) NE 0 THEN self.callbackObj = callbackObj

IF N_ELEMENTS( callbackMethod ) NE 0 THEN self.callbackMethod = callbackMethod

END

PRO TwinViewer__define

define = { TwinViewer, $
           column: 0B, $
           row: 0B, $
           panelXSize: 0, $
           panelYSize: 0, $
           title: '', $
           tlb: 0L, $
           fuvWindow: 0L, $
           fuvWindowID: 0L, $
           fuvStretch: [0., 0.], $
           fuvHistEqual: 0B, $
           nuvWindow: 0L, $
           nuvWindowID: 0L, $
           nuvStretch: [0., 0.], $
           nuvHistEqual: 0B, $
           callbackObj: OBJ_NEW(), $
           callbackMethod: '', $
           stretching: 0B }

END

PRO RunTwinViewer, column=column, group_leader=group_leader, panelXSize=panelXSize, $
                   panelYSize=panelYSize, object=object, row=row, title=title

object = OBJ_NEW( 'TwinViewer', column=column, group_leader=group_leader, $
                  panelXSize=panelXSize, panelYSize=panelYSize, row=row, title=title )

END
