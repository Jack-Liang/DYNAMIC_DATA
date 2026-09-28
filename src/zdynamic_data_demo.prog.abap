REPORT zdynamic_data_demo.

* Demo and smoke test for ZCL_DYNAMIC_OBJECT (see README.md).
* It generates the types from the README examples plus a few edge
* cases and prints the resulting component trees. The program is not
* required by the library itself and can be deleted after pulling.

CLASS lcl_demo DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS run.

  PRIVATE SECTION.
    CLASS-METHODS:
      usage_field_tab,
      usage_json_default,
      usage_json_inferred,
      usage_create_data,
      edge_cases,
      create_by_json
        IMPORTING
          iv_json    TYPE string
          iv_no_type TYPE c DEFAULT abap_true
        RETURNING
          VALUE(rr_type) TYPE REF TO cl_abap_datadescr,
      create_by_tab
        IMPORTING
          it_field_tab TYPE zdot_datadescr
          iv_type      TYPE zdoe_fldtype
        RETURNING
          VALUE(rr_type) TYPE REF TO cl_abap_datadescr,
      headline
        IMPORTING
          iv_title TYPE string,
      print_type
        IMPORTING
          ir_descr TYPE REF TO cl_abap_datadescr
          iv_title TYPE string,
      print_nodes
        IMPORTING
          ir_descr  TYPE REF TO cl_abap_datadescr
          iv_indent TYPE i,
      type_text
        IMPORTING
          ir_descr    TYPE REF TO cl_abap_datadescr
        RETURNING
          VALUE(rv_text) TYPE string.

ENDCLASS.

CLASS lcl_demo IMPLEMENTATION.

  METHOD run.

    usage_field_tab( ).
    usage_json_default( ).
    usage_json_inferred( ).
    usage_create_data( ).
    edge_cases( ).

  ENDMETHOD.


  METHOD usage_field_tab.

    headline( 'Usage 1: generate through configuration fields' ).

    DATA(lt_field_tab) = VALUE zdot_datadescr(
      ( fldname = 'hello'        fldtype = 'F' )
      ( fldname = 'author'       fldtype = 'F' )
      ( fldname = 'skills'       fldtype = 'T' )
      ( fldname = 'skills-name'  fldtype = 'F' )
      ( fldname = 'skills-level' fldtype = 'F' ) ).

    print_type( ir_descr = create_by_tab( it_field_tab = lt_field_tab iv_type = 'S' )
                iv_title = 'hello / author / skills' ).

  ENDMETHOD.


  METHOD usage_json_default.

    headline( 'Usage 2: generate via json (default NO_TYPE, all strings)' ).

    DATA(lv_json) =
      `{ "hello": "hi", "author": { "name": "Jack", "favLang": "ABAP", "github": "g" },`
      && ` "skills": [ { "name": "ABAP", "level": 95 } ] }`.

    print_type( ir_descr = create_by_json( lv_json )
                iv_title = 'hello / author / skills' ).

  ENDMETHOD.


  METHOD usage_json_inferred.

    headline( 'Usage 3: generate via json with NO_TYPE = '' (inferred types)' ).

    DATA(lv_json) = `{ "level": 95, "price": 88.5, "flag": true, "note": null }`.

    print_type( ir_descr = create_by_json( iv_json = lv_json iv_no_type = abap_false )
                iv_title = 'level / price / flag / note' ).

  ENDMETHOD.


  METHOD usage_create_data.

    headline( 'Usage 4: CREATE_DATA - type plus values in one step (no /UI2 needed)' ).

    DATA(lv_json) =
      `{ "hello": "hi", "author": { "name": "Jack", "favLang": "ABAP" },`
      && ` "skills": [ { "name": "ABAP", "level": 95 } ] }`.

    CALL METHOD zcl_dynamic_object=>create_data
      EXPORTING
        json_data            = lv_json
        no_type              = abap_false
      RECEIVING
        ref_data             = DATA(lr_data)
      EXCEPTIONS
        unsupported_type     = 1
        execution_failed     = 2
        duplicate_components = 3
        invalid_json         = 4
        invalid_field_name   = 5
        OTHERS               = 6.

    IF sy-subrc <> 0.
      WRITE: / '  create_data raised exception, sy-subrc =', sy-subrc.
      RETURN.
    ENDIF.

    ASSIGN lr_data->* TO FIELD-SYMBOL(<wa>).

    ASSIGN COMPONENT 'HELLO' OF STRUCTURE <wa> TO FIELD-SYMBOL(<hello>).
    ASSIGN COMPONENT 'AUTHOR' OF STRUCTURE <wa> TO FIELD-SYMBOL(<author>).
    ASSIGN COMPONENT 'NAME' OF STRUCTURE <author> TO FIELD-SYMBOL(<author_name>).
    ASSIGN COMPONENT 'SKILLS' OF STRUCTURE <wa> TO FIELD-SYMBOL(<skills>).

    WRITE: / 'HELLO       =', <hello>.
    WRITE: / 'AUTHOR-NAME =', <author_name>.
    WRITE: / 'SKILLS:'.
    LOOP AT <skills> ASSIGNING FIELD-SYMBOL(<skill>).
      ASSIGN COMPONENT 'NAME' OF STRUCTURE <skill> TO FIELD-SYMBOL(<skill_name>).
      ASSIGN COMPONENT 'LEVEL' OF STRUCTURE <skill> TO FIELD-SYMBOL(<skill_level>).
      WRITE: / '  ', <skill_name>, <skill_level>.
    ENDLOOP.

  ENDMETHOD.


  METHOD edge_cases.

    headline( 'Edge cases' ).

    WRITE / 'Initial values (0, empty string) are kept:'.
    print_type( ir_descr = create_by_json( iv_json = `{"price": 0, "note": ""}`
                                           iv_no_type = abap_false )
                iv_title = 'price / note' ).

    WRITE / 'An empty root array becomes TABLE OF string:'.
    print_type( ir_descr = create_by_json( `[]` )
                iv_title = 'root' ).

    WRITE / 'A consistent scalar array types the table line:'.
    print_type( ir_descr = create_by_json( iv_json = `{"points": [10, 20]}`
                                           iv_no_type = abap_false )
                iv_title = 'points' ).

    WRITE / 'A key containing the hierarchy separator raises an exception:'.
    create_by_json( `{"a-b": 1}` ).

    WRITE / 'Broken json raises an exception:'.
    create_by_json( `{` ).

  ENDMETHOD.


  METHOD create_by_json.

    CALL METHOD zcl_dynamic_object=>create_main
      EXPORTING
        json_data          = iv_json
        no_type            = iv_no_type
      RECEIVING
        ref_type           = rr_type
      EXCEPTIONS
        unsupported_type     = 1
        execution_failed     = 2
        duplicate_components = 3
        invalid_json         = 4
        invalid_field_name   = 5
        OTHERS               = 6.

    IF sy-subrc <> 0.
      WRITE: / '  create_main raised exception, sy-subrc =', sy-subrc.
    ENDIF.

  ENDMETHOD.


  METHOD create_by_tab.

    CALL METHOD zcl_dynamic_object=>create_main
      EXPORTING
        field_tab          = it_field_tab
        type               = iv_type
      RECEIVING
        ref_type           = rr_type
      EXCEPTIONS
        unsupported_type     = 1
        execution_failed     = 2
        duplicate_components = 3
        invalid_json         = 4
        invalid_field_name   = 5
        OTHERS               = 6.

    IF sy-subrc <> 0.
      WRITE: / '  create_main raised exception, sy-subrc =', sy-subrc.
    ENDIF.

  ENDMETHOD.


  METHOD headline.

    SKIP.
    WRITE / iv_title.
    ULINE.

  ENDMETHOD.


  METHOD print_type.

    IF ir_descr IS NOT BOUND.
      RETURN.
    ENDIF.

    WRITE: / 'Components of', iv_title, ':'.
    print_nodes( ir_descr = ir_descr iv_indent = 1 ).

  ENDMETHOD.


  METHOD print_nodes.

    DATA lv_pad TYPE string.
    lv_pad = repeat( val = ` ` occ = iv_indent * 2 ).

    CASE ir_descr->type_kind.
      WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.

        DATA(lt_components) = CAST cl_abap_structdescr( ir_descr )->get_components( ).
        LOOP AT lt_components INTO DATA(ls_comp).
          WRITE: / lv_pad, ls_comp-name, type_text( ls_comp-type ).
          print_nodes( ir_descr = ls_comp-type iv_indent = iv_indent + 1 ).
        ENDLOOP.

      WHEN cl_abap_typedescr=>typekind_table.

        DATA(lo_line) = CAST cl_abap_tabledescr( ir_descr )->get_table_line_type( ).
        WRITE: / lv_pad, 'table of', type_text( lo_line ).
        print_nodes( ir_descr = lo_line iv_indent = iv_indent + 1 ).

    ENDCASE.

  ENDMETHOD.


  METHOD type_text.

    CASE ir_descr->type_kind.
      WHEN cl_abap_typedescr=>typekind_struct1 OR cl_abap_typedescr=>typekind_struct2.
        rv_text = 'struct'.
      WHEN cl_abap_typedescr=>typekind_table.
        rv_text = 'table'.
      WHEN OTHERS.
        DATA(lo_elem) = CAST cl_abap_elemdescr( ir_descr ).
        rv_text = |{ lo_elem->type_kind } len { lo_elem->length } dec { lo_elem->decimals }|.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  lcl_demo=>run( ).
