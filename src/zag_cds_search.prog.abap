REPORT zag_cds_search.
*&---------------------------------------------------------------------*
*& Report ZAG_CDS_SEARCH
*&---------------------------------------------------------------------*
*& Title       : CDS Search Utility
*& Author      : Paul Sencicle
*& Date        : 21/06/2022
*& Version     : 1.00
*& Transport   :
*& Description :
*&---------------------------------------------------------------------*
*& Functional Breakdown:
*&---------------------------------------------------------------------*
*& Link ddldependency & DD03L
*& Output results as an ALV
*&---------------------------------------------------------------------*
*& Revison History:
*&---------------------------------------------------------------------*
*& Version     :
*& Author      :
*& Date        :
*& Transport   :
*& Description :
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Tables:
*----------------------------------------------------------------------*
TABLES: sscrfields,
        ddldependency,
        dd03l.
*----------------------------------------------------------------------*
* Types:
*----------------------------------------------------------------------*
TYPES: BEGIN OF gty_output,
         ddlname    TYPE ddldependency-ddlname,
         objectname TYPE ddldependency-objectname,
*        state      TYPE ddldependency-state,
         objecttype TYPE ddldependency-objecttype,
         tabname    TYPE dd03l-tabname,
         fieldname  TYPE dd03l-fieldname,
         keyflag    TYPE dd03l-keyflag,
         mandatory  TYPE dd03l-mandatory,
         rollname   TYPE dd03l-rollname,
       END OF gty_output.

TYPES gty_output_t TYPE STANDARD TABLE OF gty_output.
*----------------------------------------------------------------------*
* Data:
*----------------------------------------------------------------------*
DATA: gt_output    TYPE gty_output_t,
      gs_variant   TYPE disvariant,
      gs_vari      TYPE slis_vari,
      go_dock      TYPE REF TO cl_gui_docking_container,
      go_cont      TYPE REF TO cl_gui_container,
      go_alv       TYPE REF TO cl_salv_table,
      go_functions TYPE REF TO cl_salv_functions_list,
      gv_repid     TYPE sy-repid,
      gv_dynnr     TYPE sy-dynnr,
      go_selection TYPE REF TO cl_salv_selections,
      go_layout    TYPE REF TO cl_salv_layout,
      go_display   TYPE REF TO cl_salv_display_settings,
      go_cols      TYPE REF TO cl_salv_columns,
      go_column    TYPE REF TO cl_salv_column_list,
      gs_key       TYPE salv_s_layout_key,
      gv_icon      TYPE string,
      gv_tooltip   TYPE string,
      gv_title     TYPE lvc_title.

CONSTANTS c_handle_init TYPE disvariant-handle VALUE ''.
*----------------------------------------------------------------------*
* Local Class Definitions:
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Local Class Implementations:
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Selection Screen:
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-bl1.
  SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-bl2.
    SELECT-OPTIONS: s_ddlnam FOR ddldependency-ddlname,
                    s_tabnam FOR dd03l-tabname,
                    s_fldnam FOR dd03l-fieldname,
                    s_rolnam FOR dd03l-rollname.
  SELECTION-SCREEN END OF BLOCK bl2.
  SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE TEXT-bl3.
    PARAMETERS p_layout TYPE disvariant-variant.
  SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN END OF BLOCK bl1.
*----------------------------------------------------------------------*
* Events:
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM get_alv_variant
    USING
     c_handle_init
    CHANGING
     gs_variant
     p_layout.

AT SELECTION-SCREEN.
*  PERFORM at_selection_screen.
*----------------------------------------------------------------------*
* Main Processing:
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM read_data.

END-of-SELECTION.
  PERFORM output_report.
*----------------------------------------------------------------------*
* Subroutines:
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LAYOUT   text
*      -->P_HANDLE   text
*      <--P_VARIANT  text
*----------------------------------------------------------------------*
FORM set_layout  USING    p_layout  TYPE disvariant-variant
                          p_handle  TYPE disvariant-handle
                 CHANGING p_variant TYPE disvariant.

  DATA ls_variant TYPE disvariant.

  CLEAR p_variant.
  IF NOT p_layout IS INITIAL.
    ls_variant-report   = sy-repid.
    ls_variant-username = sy-uname.
    ls_variant-handle   = p_handle.
    MOVE p_layout TO ls_variant-variant.
* Check Variant Exists:
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save        = 'A'
      CHANGING
        cs_variant    = ls_variant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
* Error Invalid Variant:
      CLEAR p_variant.
    ELSE.
      MOVE ls_variant TO p_variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  get_alv_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VARIANT  text
*      -->P_HANDLE   text
*      -->P_LAYOUT   text
*----------------------------------------------------------------------*
FORM get_alv_variant USING    p_handle  TYPE disvariant-handle
                     CHANGING p_variant TYPE disvariant
                              p_layout  TYPE disvariant-variant.

  DATA lv_rc TYPE c.

  p_variant-report = sy-repid.
  p_variant-handle = p_handle.
  p_variant-username = sy-uname.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = p_variant
      i_save        = 'A'
    IMPORTING
      e_exit        = lv_rc
      es_variant    = p_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.                                    "#EC *
  IF lv_rc IS INITIAL.           "dialog not cancelled
    p_layout = p_variant-variant.
  ENDIF.

ENDFORM.                    " get_alv_variant
*&---------------------------------------------------------------------*
*& Form read_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM read_data.

* Status message:
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Reading CDS dependencies...'.
* DB Read:
  SELECT a~ddlname a~objectname a~objecttype
         b~tabname b~fieldname b~keyflag b~mandatory b~rollname
    INTO TABLE gt_output
    FROM ddldependency AS a INNER JOIN dd03l AS b
      ON b~tabname = a~objectname
     AND b~as4local = a~state
   WHERE a~ddlname   IN s_ddlnam
     AND b~tabname   IN s_tabnam
     AND b~fieldname IN s_fldnam
     AND b~rollname  IN s_rolnam.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form output_report
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM output_report.

  WRITE 'CDS Search'.
* Add selections at the top of the screen...

* Check we have results returned from the search:
  IF gt_output IS INITIAL.
    NEW-LINE.
    SKIP.
    WRITE '*** No Results ***'.
    RETURN.
  ENDIF.
* Set the container screen and program:
  gv_repid = 'SAPMSSY0'.
  gv_dynnr = '0120'.
* Create a docking control at bottom
  CHECK go_dock IS INITIAL.
  CREATE OBJECT go_dock
    EXPORTING
      repid = gv_repid
      dynnr = gv_dynnr
      ratio = 80
      side  = cl_gui_docking_container=>dock_at_bottom
      name  = 'DOCK_CONT'.
  IF sy-subrc <> 0.
    MESSAGE 'Error in the Docking control' TYPE 'S'.
  ENDIF.
* Create a SALV for output
  CHECK go_alv IS INITIAL.
  TRY.
* Narrow Casting: To initialize custom container from docking container
      go_cont ?= go_dock.
* SALV Table Display on the Docking container
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display   = if_salv_c_bool_sap=>false
          r_container    = go_cont
          container_name = 'DOCK_CONT'
        IMPORTING
          r_salv_table   = go_alv
        CHANGING
          t_table        = gt_output.
    CATCH cx_salv_msg.
  ENDTRY.

* Pf status
  go_functions = go_alv->get_functions( ).
  go_functions->set_default( abap_true ).
* Get the display settings:
  go_display = go_alv->get_display_settings( ).
  go_display->set_fit_column_to_table_size( value = if_salv_c_bool_sap=>true ).
* Get layout object
  go_layout = go_alv->get_layout( ).
* Set Layout save restriction
* Set Layout Key .. Unique key identifies the Different ALVs
  gs_key-report = sy-repid.
  go_layout->set_key( gs_key ).
* Allow user to save the layout as the default:
  go_layout->set_default( abap_true ).
* Set the layout if provided in the parameters:
  IF NOT gs_variant-variant IS INITIAL.
    gs_vari = gs_variant-variant.
* Set the initial variant:
    go_layout->set_initial_layout( gs_vari ).
  ENDIF.
* Remove Save layout restriction.
  go_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
* Output display
  go_alv->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form at_selection_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM at_selection_screen.

  IF sscrfields-ucomm = 'ONLI' OR
     sscrfields-ucomm = 'SJOB'.
    PERFORM set_layout
      USING
       p_layout
       c_handle_init
      CHANGING
       gs_variant.
  ENDIF.

ENDFORM.
