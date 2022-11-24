*&---------------------------------------------------------------------*
*& Report ZHTTP_API_CALL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhttp_api_call MESSAGE-ID zmm.

TYPES: BEGIN OF ty_createshipment_req,
*         _carrier_i_d                 TYPE string,
         carrierid                 TYPE string,
         carrierservicecode        TYPE string,
         printerid                 TYPE string,  "Printer Name
* Need to add in:
*        printerdrivername         TYPE string,
*        printerportnumber         TYPE string,

         numberoflabels            TYPE string,
         recipientname             TYPE string,
         recipientemail            TYPE string,
         recipientmobile           TYPE string,
         recipientaddressline1     TYPE string,
         recipientaddressline2     TYPE string,
         recipientaddressline3     TYPE string,
         recipientaddresscity      TYPE string,
         recipientaddresspostcode  TYPE string,
         recipientaddresscountry   TYPE string,
         deliverynumber            TYPE string,
         collectionname            TYPE string,
         collectionemail           TYPE string,
         collectionmobile          TYPE string,
         collectionaddressline1    TYPE string,
         collectionaddressline2    TYPE string,
         collectionaddressline3    TYPE string,
         collectionaddresscity     TYPE string,
         collectionaddresspostcode TYPE string,
         collectionaddresscountry  TYPE string,
         strquarterpallets         TYPE string,
         strhalfpallets            TYPE string,
         strfullpallets            TYPE string,
         strtotalweight            TYPE string,
         consignmentid             TYPE string,
         manifestdate              TYPE string,
         deliverydate              TYPE string,
         carriershipmentid         TYPE string,  "Might need this for Expect...
       END OF ty_createshipment_req.

TYPES ty_createshipment_req_t TYPE STANDARD TABLE OF ty_createshipment_req WITH DEFAULT KEY.

TYPES: BEGIN OF ty_createshipment_resp,
         carriershipmentid TYPE string,
         labelprintstatus  TYPE string,
         errormessage      TYPE string,
       END OF ty_createshipment_resp.

DATA: gv_tvarvc_name       TYPE tvarvc-name VALUE 'TEST_BOOMI_API_GET_URL',
      gv_tvarvc_auth       TYPE tvarvc-name VALUE 'TEST_BOOMI_API_AUTH', "B64 encoded username & password for BOOMI
      go_http_client       TYPE REF TO if_http_client,
      gv_response_body     TYPE string,
      gv_authorisation     TYPE string,
      gv_http_status_code  TYPE i,
      gv_http_status_text  TYPE string,
      gv_request_body_json TYPE string.

CONSTANTS: gc_method_post TYPE string VALUE 'POST',
           gc_method_get  TYPE string VALUE 'GET',
           gc_auth_field  TYPE string VALUE 'Authorization',
           gc_ctyp_field  TYPE string VALUE 'Content-Type',
           gc_appl_json   TYPE string VALUE 'application/json'.


PARAMETERS: p_vbeln TYPE likp-vbeln MATCHCODE OBJECT vmvl,
            p_scode TYPE text20 DEFAULT 'SND',
            p_nlabs TYPE numc2  DEFAULT 1,
            p_prnid TYPE char30 DEFAULT 'PrinterCF01'.

* Attempt to call BOOMI API - Post req. (Convert to a template method):
START-OF-SELECTION.

  BREAK senciclep.

  PERFORM call_rest_api_send_url. " Point to the URL:
  PERFORM set_post_method.        " Set the GET/POST method:
  PERFORM get_basic_auth.         " Get Basic Auth:
  PERFORM build_request_header.   " Set request headers:
  PERFORM build_request_body.     " Build the API body:
  PERFORM call_send_request.      " Send the request:
  PERFORM call_receive_response.  " Receive the response:
  PERFORM get_response_status.    " Get the response status:
  PERFORM get_response_body.      " Get the response body:
  PERFORM output_response.


*----------------------------------------------------------------------*
* Subroutines:
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form call_rest_api_send_url
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM call_rest_api_send_url.

* Get the URL from TVARVC (This needs to be created):

  DATA lv_send_url TYPE string.

  SELECT SINGLE low
    INTO lv_send_url
    FROM tvarvc
   WHERE name = gv_tvarvc_name
     AND type = 'P'.
  IF sy-subrc = 0.
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_send_url
      IMPORTING
        client             = go_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        pse_not_found      = 4
        pse_not_distrib    = 5
        pse_errors         = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_post_method
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM set_post_method.

* Set the 'POST' method:
  go_http_client->request->set_method( method = gc_method_post ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_request_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM build_request_header.

* Set the authorisation value pair in the header:
  PERFORM set_header_field
    USING
     gc_auth_field
     gv_authorisation.
* Set the content type value pair in the header:
  PERFORM set_header_field
    USING
     gc_ctyp_field
     gc_appl_json.

*  PERFORM set_header_field
*    USING
*     'Accept'
*     '*/*'.
*
*  PERFORM set_header_field
*    USING
*     'Accept-Encoding'
*     'gzip, deflate, br'.
*
*  PERFORM set_header_field
*    USING
*     'Connection'
*     'keep-alive'.

* Set the BOOMI debug value pair:
  PERFORM set_header_field
    USING
     'X-Boomi-Debug'
     'True'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_header_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_ATTR_NAM
*&      --> P_ATTR_VAL
*&---------------------------------------------------------------------*
FORM set_header_field  USING p_attr_nam TYPE string
                             p_attr_val TYPE string.

  go_http_client->request->set_header_field(
    EXPORTING
      name  = p_attr_nam    " Name of the header field
      value = p_attr_val ). " HTTP header field value

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_basic_auth
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_basic_auth.

  SELECT SINGLE low
    INTO gv_authorisation
    FROM tvarvc
   WHERE name = gv_tvarvc_auth
     AND type = 'P'.
  IF sy-subrc = 0.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_request_body
*&---------------------------------------------------------------------*
*& Build the request body (here in JSON)
*&---------------------------------------------------------------------*
FORM build_request_body.

  TYPES: BEGIN OF ty_data,
           d TYPE ty_createshipment_req_t,
         END OF ty_data.

  DATA: lv_json               TYPE string VALUE '{"test":"testy"}',
        ls_createshipment_req TYPE ty_createshipment_req,
        lt_createshipment_req TYPE ty_createshipment_req_t,
        lv_shipment_request   TYPE ty_data.

*--------------------------------------------------------------------*
* Get delivery data and build deep structure:
*--------------------------------------------------------------------*

  PERFORM get_shipment_info
    USING
     p_vbeln
    CHANGING
     ls_createshipment_req.

  BREAK senciclep.
* Temp for testing:
*  ls_createshipment_req-recipientaddresspostcode = '123 345'.

  APPEND ls_createshipment_req TO lt_createshipment_req.
  lv_shipment_request-d = lt_createshipment_req.
* Serialise the shipment info. structure as a JSON string, for the
* POST request body:
  /ui2/cl_json=>serialize( EXPORTING
*                            data        = ls_createshipment_req
*                           data        = lt_createshipment_req
                                 data = lv_shipment_request
*                           pretty_name = /ui2/cl_json=>pretty_mode-low_case
                            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                           RECEIVING
                            r_json      = lv_json ).





*  CarrierID
*  CarrierServiceCode
*  PrinterID
*  NumberOfLabels
*  RecipientName
*  RecipientEmail
*  RecipientMobile
*  RecipientAddressLine1
*  RecipientAddressLine2
*  RecipientAddressLine3
*  RecipientAddressCity
*  RecipientAddressPostcode
*  RecipientAddressCountry
*  DeliveryNumber
*  CollectionName
*  CollectionEmail
*  CollectionMobile
*  CollectionAddressLine1
*  CollectionAddressLine2
*  CollectionAddressLine3
*  CollectionAddressCity
*  CollectionAddressPostcode
*  CollectionAddressCountry
*  StrQuarterPallets
*  StrHalfPallets
*  StrFullPallets
*  StrTotalWeight
*  ConsignmentId
*  ManifestDate
*  DeliveryDate
*  CarrierShipmentID

  BREAK senciclep.

* Build JSON body:
  go_http_client->request->set_cdata(
    EXPORTING
      data   = lv_json                 " Character data
*     offset = 0                       " Offset into character data
*     length = -1                      " Length of character data
  ).

* For use with output:
  gv_request_body_json = lv_json.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form call_send_request
*&---------------------------------------------------------------------*
*& send the request
*&---------------------------------------------------------------------*
FORM call_send_request.

  go_http_client->send(
*  EXPORTING
*    timeout                    = co_timeout_default " Timeout of Answer Waiting Time
   EXCEPTIONS
     http_communication_failure = 1                  " Communication Error
     http_invalid_state         = 2                  " Invalid state
     http_processing_failed     = 3                  " Error when processing method
     http_invalid_timeout       = 4                  " Invalid Time Entry
     OTHERS                     = 5 ).
  IF sy-subrc <> 0.
    IF sy-msgty IS INITIAL.
      MESSAGE e000 WITH 'Communication Error'.
    ELSE.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form call_receive_response
*&---------------------------------------------------------------------*
*& Receive the response
*&---------------------------------------------------------------------*
FORM call_receive_response.

  go_http_client->receive(
    EXCEPTIONS
      http_communication_failure = 1                " Communication Error
      http_invalid_state         = 2                " Invalid state
      http_processing_failed     = 3                " Error when processing method
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
    IF sy-msgty IS INITIAL.
      MESSAGE i000 WITH 'Communication Error'.
    ELSE.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_response_status
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_response_status.

  go_http_client->response->get_status(
    IMPORTING
      code   = gv_http_status_code               " HTTP Status Code
      reason = gv_http_status_text ).            " HTTP status description

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_response_body
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_response_body.

  DATA: ls_createshipment_resp TYPE ty_createshipment_resp.

  gv_response_body = go_http_client->response->get_cdata( ).
* Attempt to convert this:

* Deserialise the reponse:
  /ui2/cl_json=>deserialize( EXPORTING json = gv_response_body CHANGING data = ls_createshipment_resp ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form output_response
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM output_response.

  BREAK senciclep.
* Add request payload to response body:
  gv_response_body = gv_response_body && ' ' && gv_request_body_json.

* Output the response:
*WRITE:/ 'REST API Return Code/Text:', gv_http_status_code, '/', gv_http_status_text.
  cl_abap_browser=>show_html(
    EXPORTING
      title        = 'API Response'                      " Window Title
      html_string  = gv_response_body ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_shipment_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_VBELN
*&      <-- P_S_CREATESHIPMENT_REQ
*&---------------------------------------------------------------------*
FORM get_shipment_info  USING    p_vbeln                TYPE vbeln
                        CHANGING p_s_createshipment_req TYPE ty_createshipment_req.

  DATA: lv_lfdat_c TYPE c LENGTH 10,
        lv_nlabs   TYPE i.

  CLEAR p_s_createshipment_req.

* CDS: I_DELIVERYDOCUMENT as base for a consumption view that can be used here!!
* CDS: Z_C_CARRIER_DELIVERY

*  SELECT SINGLE lfdat
*    INTO @DATA(lv_lfdat)
*    FROM likp
*   WHERE vbeln = @p_vbeln.

* Get delivery carrier info. from CDS consumption view:
  SELECT SINGLE *
    FROM z_c_carrier_delivery
    INTO @DATA(ls_carrier_delivery)
   WHERE deliverydocument = @p_vbeln.
  IF sy-subrc = 0.

* Printer ID
* Service code
* Carrier ID will come from the delivery (or related bespoke table)
* Shipment ID will be updated here

*  CarrierID              - Carrier from delivery partners
    p_s_createshipment_req-carrierid = ls_carrier_delivery-carrierid.
*p_s_createshipment_req-CarrierID = ls_carrier_delivery-carriername1.
    p_s_createshipment_req-printerid = p_prnid.
*  CarrierServiceCode     -
    p_s_createshipment_req-carrierservicecode = p_scode.
*  PrinterID              - parameter
    p_s_createshipment_req-printerid = p_prnid.
*  NumberOfLabels         - parameter
    MOVE p_nlabs TO lv_nlabs.
    p_s_createshipment_req-numberoflabels = lv_nlabs.
*  RecipientName          - Ship-to name
    p_s_createshipment_req-recipientname = ls_carrier_delivery-recipientname1.
*  RecipientEmail         - Ship-to email
    p_s_createshipment_req-recipientemail = ls_carrier_delivery-recipientemailaddress.
*  RecipientMobile        - Ship-to mobile
    p_s_createshipment_req-recipientmobile = ls_carrier_delivery-recipientmobilephonenumber.
*  RecipientAddressLine1  - Address line 1
    p_s_createshipment_req-recipientaddressline1 = ls_carrier_delivery-recipientstreetprefixname.
*  RecipientAddressLine2  - Address line 2
    p_s_createshipment_req-recipientaddressline2 = ls_carrier_delivery-recipienthousenumber.
*  RecipientAddressLine3  - Address line 3
    p_s_createshipment_req-recipientaddressline3 = ls_carrier_delivery-recipientstreetname.
*  RecipientAddressCity
    p_s_createshipment_req-recipientaddresscity = ls_carrier_delivery-recipientcityname.
*  RecipientAddressPostcode
    p_s_createshipment_req-recipientaddresspostcode = ls_carrier_delivery-recipientpostalcode.
*  RecipientAddressCountry
    p_s_createshipment_req-recipientaddresscountry = ls_carrier_delivery-recipientcountry.
*  DeliveryNumber          - Vbeln
    p_s_createshipment_req-deliverynumber = p_vbeln.
*  CollectionName
    p_s_createshipment_req-collectionemail = ls_carrier_delivery-senderemailaddress.
*  CollectionMobile
*  CollectionAddressLine1
*  CollectionAddressLine2
*  CollectionAddressLine3
*  CollectionAddressCity
    p_s_createshipment_req-collectionaddresspostcode = ls_carrier_delivery-senderpostalcode.
*  CollectionAddressCountry
    p_s_createshipment_req-collectionaddresscountry = ls_carrier_delivery-sendercountry.
*  StrQuarterPallets
*  StrHalfPallets
*  StrFullPallets
*  StrTotalWeight
*  ConsignmentId
*  ManifestDate
*  DeliveryDate             - Delivery date
    WRITE ls_carrier_delivery-deliverydate TO lv_lfdat_c.
    p_s_createshipment_req-deliverydate = lv_lfdat_c.
*  CarrierShipmentID
  ENDIF.

ENDFORM.
