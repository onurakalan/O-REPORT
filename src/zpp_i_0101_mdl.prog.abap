MODULE status_0100 OUTPUT.
  SET PF-STATUS 'S100'.
  SET TITLEBAR 'T100'.
ENDMODULE.

MODULE prepare_alv_0100 OUTPUT.
  gr_report_view = lcl_report_view=>get_instance( ).
  gr_report_view->display_data( ).
ENDMODULE.

MODULE user_command_0100 INPUT.
  gr_event_handler->handle_user_command(
      e_ucomm = okcode
  ).
  CLEAR okcode.
ENDMODULE.

MODULE exit_command INPUT.
  CLEAR : okcode.
  LEAVE TO SCREEN 0.
ENDMODULE.
