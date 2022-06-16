**---Class Definition-------------------------------------------------*
CLASS :
  lcl_event_handler DEFINITION DEFERRED,
  lcl_report_view   DEFINITION DEFERRED,
  lcl_main          DEFINITION DEFERRED.

**---Data Declaration-------------------------------------------------*
DATA :
  gr_main          TYPE REF TO lcl_main,
  gr_event_handler TYPE REF TO lcl_event_handler,
  gr_report_view   TYPE REF TO lcl_report_view.

DATA :
  okcode TYPE sy-ucomm.
