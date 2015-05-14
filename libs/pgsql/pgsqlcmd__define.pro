;+
; NAME:
;     pgSqlCmd
;
; PURPOSE:
;     A PostgreSQL database command object.  Wraps libpq DLM functions.
;
; TYPE:
;     OBJECT
;
; CATEGORY:
;     Data structures, Database
;
; CALLING SEQUENCE:
;     You probably don't want to go creating these directly.  They are
;     most likely to be encountered when working with the pgSql object.
;
;     myRes = OBJ_NEW ('pgSqlCmd')
;
; INPUTS:
;     NONE
;
; KEYWORDS:
;     NONE
;
; DEPENDENCIES:
;     NONE
;
; PUBLIC METHODS:
;     
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;
;     1.0 RHS - Jan 2005 Creation
;     1.1 RHS - Feb 2005 Removed unnecessary and buggy typelist
;-

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::nTuples
;
; PURPOSE:
;   Returns the number of tuples in the result
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   The number of tuples
;
; TYPE:
;   public
;
;-
function pgSqlCmd::nTuples

  compile_opt idl2

  return, self.nTuples
  
end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::nFields
;
; PURPOSE:
;   Returns the number of fields in the result
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   The number of tuples
;
; TYPE:
;   public
;
;-
function pgSqlCmd::nFields

  compile_opt idl2

  return, self.nFields
  
end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::GetType
;
; PURPOSE:
;   Converts a the PG type to an IDL data type
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   None
;
; TYPE:
;   public
;
;-
function pgSqlCmd::GetType, PgOid

  compile_opt idl2

  n = n_elements(PgOid)
  ret = bytarr(n)

  for i=0, n-1 do begin

    ; Map PostgreSQL Oid values to IDL types (everything else is a string)
    case PgOid[i] of
        16: ret[i] = 1  ; bool = IDL byte
        17: ret[i] = 1  ; bytea = IDL byte
        20: ret[i] = 3  ; int8 = IDL long
        21: ret[i] = 2  ; int2 = IDL integer
        23: ret[i] = 3  ; int4 = IDL long
        26: ret[i] = 3  ; oid = IDL long
        27: ret[i] = 3  ; tid = IDL long
        28: ret[i] = 3  ; xid = IDL long
        29: ret[i] = 3  ; cid = IDL long
       700: ret[i] = 4  ; float4 = IDL float
       701: ret[i] = 5  ; float8 = IDL double
      1700: ret[i] = 5  ; numeric = IDL double
      else: ret[i] = 7  ; unknown = IDL string
    endcase

  endfor
  
  return, ret

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::GetField
;
; PURPOSE:
;   Gets fields from the result
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   field - Either a numeric or string corresponding to the field name
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   The resulting array (1 or 2 dimensions)
;
; TYPE:
;   public
;
;-
function pgSqlCmd::GetField, field, _Extra=e

  compile_opt idl2

  catch, error_status
  if ( error_status ne 0 ) then begin
    catch, /cancel
    message, !error_state.msg, /continue
    return, -1
  endif    

  np = n_params()

  ; First argument
  if (np ne 1) then message, "Must pass field index or name"

  ; Check the result
  if (self->Status(_Extra=e) ne 1) then $
    message, "Error in result (use verbose flag for details)"

  ; Check there are tuples
  if (self.nTuples lt 1) then message, "No tuples"

  ; Get the field index if we passed a string
  if size(field, /type) eq 7 then $
    field_id = PGSQL_PQfnumber(self.result_idx, field) $
  else $
    field_id = field

  ; Check the field number
  idx = where(field_id lt 0, count)
  if (count gt 0) then $
    message, "Field(s) not found " + field[idx]

  ; Get the resulting array
  PGSQL_PQgetvalue, self.result_idx, lindgen(self.nTuples), field_id, table

  ; Determine the data type of the columns
  type = self->GetType(PGSQL_PQftype(self.result_idx, field_id))

  ; Only convert them if they are all the same type
  if ( total(type eq type[0]) eq n_elements(type) ) then begin

    case type[0] of
      1: table = byte(table)
      2: table = fix(table)
      3: table = long(table)
      4: table = float(table)
      5: table = double(table)
      6: table = complex(table)
      9: table = dcomplex(table)
     12: table = uint(table)
     13: table = ulong(table)
     14: table = long64(table)
     15: table = ulong64(table)
     else: ; do nothing
    endcase

  endif

  ; Turn a [1, n] array into [n] array (save troubles later)
  if (size(table, /n_dim) eq 2) then $
    table = transpose(temporary(table))

  return, table
  
end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::Status
;
; PURPOSE:
;   Get the status of the result
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   0/1
;
; TYPE:
;   public
;
;-
function pgSqlCmd::Status, VERBOSE=verbose

  compile_opt idl2

  if keyword_set(verbose) then verbose = 1 else verbose = 0

  status = 1
  status_str = PGSQL_PQresStatus(self.result_idx)

  if ( stregex(status_str, "COMMAND_OK$|TUPLES_OK$", /bool) ne 1 ) then begin

    status = 0

    if (verbose eq 1) then begin
      error = PGSQL_PQresultErrorMessage(self.result_idx)
      message, "Result status is " + status_str + " (" + error + ")", /CONTINUE
    endif

  endif

  return, status

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::Execute
;
; PURPOSE:
;   Execute an SQL command
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   connection - the PostgreSQL connection to use
;   command - the SQL command to set
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   The number of tuples or -1
;
; TYPE:
;   public
;
;-
function pgSqlCmd::Execute, command, _EXTRA=e

  compile_opt idl2

  catch, error_status
  if ( error_status ne 0 ) then begin
    catch, /cancel
    message, !error_state.msg, /continue
    return, -1
  endif    

  np = n_params()

  ; First argument
  if (np gt 0) then self.command = command

  ; Check if we are ready to execute
  if (ptr_valid(self.connection) ne 1) then $
    message, "Connection object is not valid"

  if (*self.connection->Status(_Extra=e) ne 1) then $
    message, "Error in connection (use verbose flag for details)"

  if (strcmp(self.command, "") eq 1) then message, "No SQL command given"

  ; Clear any previous result
  self->Clear

  ; Execute the query
  self.result_idx = PGSQL_PQexec(*self.connection->GetIndex(), self.command)

  ; Check the result
  if (self->Status(_Extra=e) ne 1) then $
    message, "Error in result (use verbose flag for details)"

  ; The number of tuples
  if stregex(command, '^SELECT', /BOOLEAN) then $
    self.nTuples = PGSQL_PQntuples(self.result_idx) $
  else $
    self.nTuples = PGSQL_PQcmdTuples(self.result_idx)

  ; The number of fields
  self.nFields = PGSQL_PQnfields(self.result_idx)

  return, self.nTuples

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::Clear
;
; PURPOSE:
;   Clears the result
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   None
;
; TYPE:
;   public
;
;-
pro pgSqlCmd::Clear

  compile_opt idl2

  if (self.result_idx gt 0) then begin 
    PGSQL_PQclear, self.result_idx
    self.result_idx = 0
  endif

  self.nFields = 0
  self.nTuples = 0

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::GetIndex
;
; PURPOSE:
;   Returns the internal result index
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; OUTPUT:
;   Command index
;
; TYPE:
;   private
;
;-
function pgSqlCmd::GetIndex
  
  compile_opt idl2, hidden

  return, self.result_idx

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::Cleanup
;
; PURPOSE:
;   Cleanup a pgSqlCmd object
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; INPUT KEYWORDS:
;   None
;
; TYPE:
;   private
;
;-
pro pgSqlCmd::Cleanup

  compile_opt idl2, hidden

  self->Clear

  if ptr_valid(self.connection) then ptr_free, self.connection

end

;--------------------------------------------------------------------------
; METHODNAME:
;   pgSqlCmd::Init
;
; PURPOSE:
;   Initializes a PostgreSQL database result object
;
; REQUIRED INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   connection - the PostgreSQL connection to use
;   command - the SQL command to set
;
; INPUT KEYWORDS:
;   None
;
; TYPE:
;   private
;
;-
function pgSqlCmd::init, connection, command, execute=execute

  compile_opt idl2, hidden

  if keyword_set(execute) then execute = 1 else execute = 0

  np = n_params()

  ; First argument
  if (np gt 0) then self.connection = ptr_new(connection)

  ; Second argument
  if (np gt 1) then self.command = command

  ; Default result index indicating no result.
  self.result_idx = 0

  ; Attempt to execute
  if ((np eq 2) and (execute eq 1)) then tmp = self->Execute()

  return, 1

end

;--------------------------------------------------------------------------
; CLASSNAME:
;   pgSqlCmd
;
; PURPOSE:
;   Defines a pgSqlCmd object
;
;-
pro pgSqlCmd__define

  compile_opt idl2, hidden

  s2 = { pgSqlCmd,               $  ; The class name
         connection: ptr_new(),  $  ; The database connection
         command: "",            $  ; The SQL command
         nTuples: 0L,            $  ; The number of result tuples
         nFields: 0L,            $  ; The number of result fields
         result_idx: 0L          $  ; Result index
       }

end
