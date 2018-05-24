METHOD if_http_extension~handle_request.

  DATA: lv_path TYPE string,
        lv_request_method TYPE string,
        lv_request_data TYPE string,
        lv_response_data TYPE string,
        lv_response_status TYPE i,
        lv_resp_stat_string TYPE string.

* get the request attributes
  lv_path = server->request->get_header_field( name = '~path_info' ).
  lv_request_method = server->request->get_header_field( name = '~request_method' ).
  lv_request_data = server->request->get_cdata( ).

  CASE lv_path.

    WHEN '/createEmployee'.
      IF lv_request_method = 'POST'.

        create_employee(
          EXPORTING
            import_data = lv_request_data
          IMPORTING
            response_data = lv_response_data ).

        server->response->set_cdata( data = lv_response_data ).

      ELSE.
        server->response->set_header_field( name = 'Allow' value = 'POST' ).
        server->response->set_status( code = '405' reason = 'Method not allowed' ).
      ENDIF.

    WHEN '/employeeMasterData'.
      IF lv_request_method = 'POST'.

        employee_master_data(
          EXPORTING
            import_data = lv_request_data
          IMPORTING
            response_data = lv_response_data ).

        server->response->set_cdata( data = lv_response_data ).

      ELSE.
        server->response->set_header_field( name = 'Allow' value = 'POST' ).
        server->response->set_status( code = '405' reason = 'Method not allowed' ).
      ENDIF.

    WHEN '/createAbsence'.
      IF lv_request_method = 'POST'.

        create_absence(
          EXPORTING
            import_data = lv_request_data
          IMPORTING
            response = lv_response_data ).

        server->response->set_cdata( data = lv_response_data ).

      ELSE.
        server->response->set_header_field( name = 'Allow' value = 'POST' ).
        server->response->set_status( code = '405' reason = 'Method not allowed' ).
      ENDIF.

    WHEN OTHERS.

      server->response->set_status( code = '404' reason = 'Not Found' ).
      lv_resp_stat_string = 'Uneli ste pogrešan PATH!'.
      server->response->set_cdata( data = lv_resp_stat_string ).

  ENDCASE.

ENDMETHOD.