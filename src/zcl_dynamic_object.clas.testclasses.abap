"* Unit tests for the local JSON parser / walker and the public API
"* of ZCL_DYNAMIC_OBJECT=>CREATE_MAIN.

CLASS ltcl_json_parser DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS parse_object FOR TESTING RAISING zcx_dynamic_json_error.
    METHODS bool_is_not_a_string FOR TESTING RAISING zcx_dynamic_json_error.
    METHODS empty_array FOR TESTING RAISING zcx_dynamic_json_error.
    METHODS array_items_get_index_names FOR TESTING RAISING zcx_dynamic_json_error.
    METHODS nested_path FOR TESTING RAISING zcx_dynamic_json_error.
    METHODS broken_json_raises FOR TESTING.
ENDCLASS.

CLASS ltcl_json_parser IMPLEMENTATION.

  METHOD parse_object.

    DATA(lt_nodes) = lcl_json_parser=>parse( `{"a": 1}` ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( lt_nodes )
      msg = 'root object plus one member expected' ).

    READ TABLE lt_nodes INDEX 1 ASSIGNING FIELD-SYMBOL(<root>).
    cl_abap_unit_assert=>assert_equals( exp = '' act = <root>-path msg = 'root path' ).
    cl_abap_unit_assert=>assert_equals( exp = '' act = <root>-name msg = 'root name' ).
    cl_abap_unit_assert=>assert_equals( exp = lcl_json_parser=>c_kind-object act = <root>-kind ).

    READ TABLE lt_nodes INDEX 2 ASSIGNING FIELD-SYMBOL(<member>).
    cl_abap_unit_assert=>assert_equals( exp = '/' act = <member>-path ).
    cl_abap_unit_assert=>assert_equals( exp = 'a' act = <member>-name ).
    cl_abap_unit_assert=>assert_equals( exp = lcl_json_parser=>c_kind-number act = <member>-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '1' act = <member>-value msg = 'number keeps source text' ).

  ENDMETHOD.

  METHOD bool_is_not_a_string.

    DATA(lt_nodes) = lcl_json_parser=>parse( `{"a": true, "b": "true"}` ).

    READ TABLE lt_nodes WITH KEY name = 'a' ASSIGNING FIELD-SYMBOL(<a>).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = lcl_json_parser=>c_kind-bool act = <a>-kind ).

    READ TABLE lt_nodes WITH KEY name = 'b' ASSIGNING FIELD-SYMBOL(<b>).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = lcl_json_parser=>c_kind-string act = <b>-kind ).

  ENDMETHOD.

  METHOD empty_array.

    DATA(lt_nodes) = lcl_json_parser=>parse( `{"list": []}` ).

    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lt_nodes ) ).

    READ TABLE lt_nodes WITH KEY name = 'list' ASSIGNING FIELD-SYMBOL(<list>).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = lcl_json_parser=>c_kind-array act = <list>-kind ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = <list>-children msg = 'empty array has no children' ).

  ENDMETHOD.

  METHOD array_items_get_index_names.

    DATA(lt_nodes) = lcl_json_parser=>parse( `[1, "x"]` ).

    READ TABLE lt_nodes INDEX 1 ASSIGNING FIELD-SYMBOL(<root>).
    cl_abap_unit_assert=>assert_equals( exp = lcl_json_parser=>c_kind-array act = <root>-kind ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = <root>-children ).

    READ TABLE lt_nodes INDEX 2 ASSIGNING FIELD-SYMBOL(<item1>).
    cl_abap_unit_assert=>assert_equals( exp = '1' act = <item1>-name ).
    cl_abap_unit_assert=>assert_equals( exp = lcl_json_parser=>c_kind-number act = <item1>-kind ).

    READ TABLE lt_nodes INDEX 3 ASSIGNING FIELD-SYMBOL(<item2>).
    cl_abap_unit_assert=>assert_equals( exp = '2' act = <item2>-name ).
    cl_abap_unit_assert=>assert_equals( exp = lcl_json_parser=>c_kind-string act = <item2>-kind ).

  ENDMETHOD.

  METHOD nested_path.

    DATA(lt_nodes) = lcl_json_parser=>parse( `{"o": {"x": 1}}` ).

    READ TABLE lt_nodes WITH KEY name = 'x' ASSIGNING FIELD-SYMBOL(<x>).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = '/o/' act = <x>-path ).

  ENDMETHOD.

  METHOD broken_json_raises.

    TRY.
        lcl_json_parser=>parse( `{` ).
        cl_abap_unit_assert=>fail( 'broken json must raise' ).
      CATCH zcx_dynamic_json_error.
        " expected
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_dynamic_type DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS readme_example_json FOR TESTING.
    METHODS readme_example_field_tab FOR TESTING.
    METHODS zero_and_empty_kept FOR TESTING.
    METHODS empty_array_member FOR TESTING.
    METHODS empty_array_root FOR TESTING.
    METHODS integer_inferred FOR TESTING.
    METHODS packed_inferred FOR TESTING.
    METHODS bool_inferred FOR TESTING.
    METHODS no_inference_by_default FOR TESTING.
    METHODS null_is_string FOR TESTING.
    METHODS array_of_scalars_typed FOR TESTING.
    METHODS union_of_array_items FOR TESTING.
    METHODS table_line_typed_from_tab FOR TESTING.
    METHODS invalid_json_raises FOR TESTING.
    METHODS invalid_field_name_raises FOR TESTING.
    METHODS dash_in_key_raises FOR TESTING.
    METHODS long_key_raises FOR TESTING.
    METHODS duplicate_after_uppercase FOR TESTING.
    METHODS invalid_root_type_raises FOR TESTING.
    METHODS metadata_constants FOR TESTING.
    METHODS create_data_fills_values FOR TESTING.
    METHODS create_data_inferred_values FOR TESTING.
    METHODS create_data_false_and_null FOR TESTING.
    METHODS create_data_array_root FOR TESTING.
    METHODS create_data_invalid_json FOR TESTING.
    METHODS walker_emits_typed_rows FOR TESTING RAISING zcx_dynamic_json_error zcx_dynamic_name_error.
    METHODS elem_c_length_diagnosis FOR TESTING.
    METHODS name_map_shortens_long_key FOR TESTING.
    METHODS name_map_invalid_target_raises FOR TESTING.
    METHODS name_map_dash_target_raises FOR TESTING.
    METHODS create_data_fills_mapped_name FOR TESTING.
    METHODS deep_nested_struct FOR TESTING.
    METHODS deep_nested_table_in_table FOR TESTING.
    METHODS create_data_deep_nested FOR TESTING.
    METHODS implicit_parent_becomes_struct FOR TESTING.
    METHODS flat_pipeline_diagnosis FOR TESTING.
    METHODS field_tab_out_of_order FOR TESTING.
    METHODS empty_struct_node_raises FOR TESTING.

    METHODS build_by_json
      IMPORTING
        json        TYPE string
        infer_types TYPE abap_bool DEFAULT abap_false
        name_map    TYPE zcl_dynamic_object=>ty_name_map OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_datadescr.

    METHODS build_data_by_json
      IMPORTING
        json        TYPE string
        infer_types TYPE abap_bool DEFAULT abap_false
        name_map    TYPE zcl_dynamic_object=>ty_name_map OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO data.

    METHODS create_data_subrc
      IMPORTING
        json TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS build_by_field_tab
      IMPORTING
        field_tab TYPE zdot_datadescr
        type      TYPE zdoe_fldtype
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_datadescr.

    METHODS create_json_subrc
      IMPORTING
        json     TYPE string
        name_map TYPE zcl_dynamic_object=>ty_name_map OPTIONAL
      RETURNING
        VALUE(result) TYPE i.

    METHODS create_field_tab_subrc
      IMPORTING
        field_tab TYPE zdot_datadescr
        type      TYPE zdoe_fldtype DEFAULT 'S'
      RETURNING
        VALUE(result) TYPE i.

    METHODS component
      IMPORTING
        struct TYPE REF TO cl_abap_structdescr
        name   TYPE string
      RETURNING
        VALUE(descr) TYPE REF TO cl_abap_datadescr.

    METHODS table_line
      IMPORTING
        descr TYPE REF TO cl_abap_datadescr
      RETURNING
        VALUE(line) TYPE REF TO cl_abap_datadescr.

    METHODS assert_string_kind
      IMPORTING
        descr TYPE REF TO cl_abap_datadescr.
ENDCLASS.

CLASS ltcl_dynamic_type IMPLEMENTATION.

  METHOD readme_example_json.

    DATA(lo_descr) = build_by_json(
      `{ "hello": "hi", "author": { "name": "Jack", "favLang": "ABAP", "github": "g" },`
      && ` "skills": [ { "name": "ABAP", "level": 95 } ] }` ).

    DATA(lo_struct) = CAST cl_abap_structdescr( lo_descr ).
    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lines( lo_struct->get_components( ) ) ).

    assert_string_kind( component( struct = lo_struct name = 'HELLO' ) ).

    " Nested object becomes a nested structure
    DATA(lo_author) = CAST cl_abap_structdescr( component( struct = lo_struct name = 'AUTHOR' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = lines( lo_author->get_components( ) ) ).
    assert_string_kind( component( struct = lo_author name = 'NAME' ) ).
    assert_string_kind( component( struct = lo_author name = 'FAVLANG' ) ).

    " Array of objects becomes a table with a structured line
    DATA(lo_skills) = CAST cl_abap_structdescr(
      table_line( component( struct = lo_struct name = 'SKILLS' ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lo_skills->get_components( ) ) ).
    assert_string_kind( component( struct = lo_skills name = 'NAME' ) ).
    " default NO_TYPE = abap_true -> everything is a string
    assert_string_kind( component( struct = lo_skills name = 'LEVEL' ) ).

  ENDMETHOD.

  METHOD readme_example_field_tab.

    DATA(lt_field_tab) = VALUE zdot_datadescr(
      ( fldname = 'hello'        fldtype = 'F' )
      ( fldname = 'author'       fldtype = 'F' )
      ( fldname = 'skills'       fldtype = 'T' )
      ( fldname = 'skills-name'  fldtype = 'F' )
      ( fldname = 'skills-level' fldtype = 'F' ) ).

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_field_tab(
      field_tab = lt_field_tab
      type      = 'S' ) ).

    assert_string_kind( component( struct = lo_struct name = 'HELLO' ) ).
    assert_string_kind( component( struct = lo_struct name = 'AUTHOR' ) ).

    DATA(lo_skills) = CAST cl_abap_structdescr(
      table_line( component( struct = lo_struct name = 'SKILLS' ) ) ).
    assert_string_kind( component( struct = lo_skills name = 'NAME' ) ).
    assert_string_kind( component( struct = lo_skills name = 'LEVEL' ) ).

  ENDMETHOD.

  METHOD zero_and_empty_kept.

    " Regression: initial values used to be dropped by IS NOT INITIAL checks
    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"price": 0, "note": ""}`
      infer_types = abap_true ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_int
      act = component( struct = lo_struct name = 'PRICE' )->type_kind ).
    assert_string_kind( component( struct = lo_struct name = 'NOTE' ) ).

  ENDMETHOD.

  METHOD empty_array_member.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json( `{"tags": []}` ) ).

    assert_string_kind( table_line( component( struct = lo_struct name = 'TAGS' ) ) ).

  ENDMETHOD.

  METHOD empty_array_root.

    " Regression: an empty root array used to dump, now it is TABLE OF string
    DATA(lo_descr) = build_by_json( `[]` ).

    cl_abap_unit_assert=>assert_bound( lo_descr ).
    assert_string_kind( table_line( lo_descr ) ).

  ENDMETHOD.

  METHOD integer_inferred.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"level": 95}`
      infer_types = abap_true ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_int
      act = component( struct = lo_struct name = 'LEVEL' )->type_kind ).

  ENDMETHOD.

  METHOD packed_inferred.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"price": 88.5}`
      infer_types = abap_true ) ).

    DATA(lo_elem) = CAST cl_abap_elemdescr( component( struct = lo_struct name = 'PRICE' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_packed
      act = lo_elem->type_kind ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_elem->decimals ).
    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = lo_elem->length
      msg = '88.5 needs 3 digits -> P length 3' ).

  ENDMETHOD.

  METHOD bool_inferred.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"flag": true}`
      infer_types = abap_true ) ).

    DATA(lo_elem) = CAST cl_abap_elemdescr( component( struct = lo_struct name = 'FLAG' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_char
      act = lo_elem->type_kind ).

    " ->length reports bytes for character types on some releases,
    " measure in character mode instead
    DATA lr_flag_data TYPE REF TO data.
    CREATE DATA lr_flag_data TYPE HANDLE lo_elem.
    ASSIGN lr_flag_data->* TO FIELD-SYMBOL(<flag>).
    DESCRIBE FIELD <flag> LENGTH DATA(lv_chars) IN CHARACTER MODE.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lv_chars ).

  ENDMETHOD.

  METHOD no_inference_by_default.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json( `{"n": 95}` ) ).

    assert_string_kind( component( struct = lo_struct name = 'N' ) ).

  ENDMETHOD.

  METHOD null_is_string.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"x": null}`
      infer_types = abap_true ) ).

    cl_abap_unit_assert=>assert_bound( component( struct = lo_struct name = 'X' ) ).
    assert_string_kind( component( struct = lo_struct name = 'X' ) ).

  ENDMETHOD.

  METHOD array_of_scalars_typed.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"points": [10, 20]}`
      infer_types = abap_true ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_int
      act = table_line( component( struct = lo_struct name = 'POINTS' ) )->type_kind ).

  ENDMETHOD.

  METHOD union_of_array_items.

    " Different keys across items are unioned into the line structure
    DATA(lo_descr) = build_by_json(
      json    = `[{"a": 1}, {"b": "x"}]`
      infer_types = abap_true ).

    DATA(lo_struct) = CAST cl_abap_structdescr( table_line( lo_descr ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lo_struct->get_components( ) ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_int
      act = component( struct = lo_struct name = 'A' )->type_kind ).
    assert_string_kind( component( struct = lo_struct name = 'B' ) ).

  ENDMETHOD.

  METHOD table_line_typed_from_tab.

    DATA(lt_field_tab) = VALUE zdot_datadescr(
      ( fldname = 'AMOUNTS' fldtype = 'T' intty = 'P' lengt = 8 decim = 2 ) ).

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_field_tab(
      field_tab = lt_field_tab
      type      = 'S' ) ).

    DATA(lo_elem) = CAST cl_abap_elemdescr( table_line(
      component( struct = lo_struct name = 'AMOUNTS' ) ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_packed
      act = lo_elem->type_kind ).
    cl_abap_unit_assert=>assert_equals( exp = 8 act = lo_elem->length ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lo_elem->decimals ).

  ENDMETHOD.

  METHOD invalid_json_raises.

    DATA(lv_subrc) = create_json_subrc( `{` ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_subrc
      msg = 'invalid_json expected' ).

  ENDMETHOD.

  METHOD invalid_field_name_raises.

    DATA(lv_subrc) = create_json_subrc( `{"a b": 1}` ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lv_subrc
      msg = 'invalid_field_name expected' ).

  ENDMETHOD.

  METHOD dash_in_key_raises.

    " '-' is the hierarchy separator and cannot be part of a key
    DATA(lv_subrc) = create_json_subrc( `{"a-b": 1}` ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lv_subrc
      msg = 'invalid_field_name expected' ).

  ENDMETHOD.

  METHOD long_key_raises.

    DATA(lv_subrc) = create_json_subrc( `{"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA": 1}` ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lv_subrc
      msg = 'invalid_field_name expected for 34 char key' ).

  ENDMETHOD.

  METHOD duplicate_after_uppercase.

    " Regression: duplicates were only detected after upper case
    " normalization moved in front of the check
    DATA(lt_field_tab) = VALUE zdot_datadescr(
      ( fldname = 'hello' fldtype = 'F' )
      ( fldname = 'HELLO' fldtype = 'F' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 3
      act = create_field_tab_subrc( lt_field_tab )
      msg = 'duplicate_components expected' ).

  ENDMETHOD.

  METHOD invalid_root_type_raises.

    " A FIELD_TAB root type other than S/T is rejected
    DATA(lt_rows) = VALUE zdot_datadescr(
      ( fldname = 'A' fldtype = 'F' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1 " unsupported_type
      act = create_field_tab_subrc( field_tab = lt_rows type = 'F' )
      msg = 'unsupported_type expected for root type F' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1 " invalid_json
      act = create_json_subrc( `` )
      msg = 'invalid_json expected for empty json' ).

  ENDMETHOD.

  METHOD metadata_constants.

    " Guards that the metadata constants are maintained (kept in sync
    " with CHANGELOG.md when releasing)
    cl_abap_unit_assert=>assert_not_initial(
      act = zcl_dynamic_object=>c_info-version
      msg = 'c_info-version must be maintained' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = zcl_dynamic_object=>c_info-author
      msg = 'c_info-author must be maintained' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = zcl_dynamic_object=>c_info-repository
      msg = 'c_info-repository must be maintained' ).
    cl_abap_unit_assert=>assert_not_initial(
      act = zcl_dynamic_object=>c_info-license
      msg = 'c_info-license must be maintained' ).

  ENDMETHOD.

  METHOD create_data_fills_values.

    DATA(lr_data) = build_data_by_json(
      `{ "hello": "hi", "skills": [ { "name": "ABAP", "level": 95 } ] }` ).

    cl_abap_unit_assert=>assert_bound( lr_data ).

    ASSIGN lr_data->* TO FIELD-SYMBOL(<wa>).

    ASSIGN COMPONENT 'HELLO' OF STRUCTURE <wa> TO FIELD-SYMBOL(<hello>).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = 'hi' act = <hello> ).

    ASSIGN COMPONENT 'SKILLS' OF STRUCTURE <wa> TO FIELD-SYMBOL(<skills>).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( <skills> ) ).

    LOOP AT <skills> ASSIGNING FIELD-SYMBOL(<skill>).
      EXIT.
    ENDLOOP.

    ASSIGN COMPONENT 'NAME' OF STRUCTURE <skill> TO FIELD-SYMBOL(<name>).
    cl_abap_unit_assert=>assert_equals( exp = 'ABAP' act = <name> ).

    " default NO_TYPE -> LEVEL is a string holding the literal text
    ASSIGN COMPONENT 'LEVEL' OF STRUCTURE <skill> TO FIELD-SYMBOL(<level>).
    cl_abap_unit_assert=>assert_equals( exp = '95' act = <level> ).

  ENDMETHOD.

  METHOD create_data_inferred_values.

    DATA(lr_data) = build_data_by_json(
      json    = `{"level": 95, "price": 88.5, "flag": true}`
      infer_types = abap_true ).

    ASSIGN lr_data->* TO FIELD-SYMBOL(<wa>).

    ASSIGN COMPONENT 'LEVEL' OF STRUCTURE <wa> TO FIELD-SYMBOL(<level>).
    cl_abap_unit_assert=>assert_equals( exp = 95 act = <level> ).

    ASSIGN COMPONENT 'PRICE' OF STRUCTURE <wa> TO FIELD-SYMBOL(<price>).
    cl_abap_unit_assert=>assert_equals( exp = '88.5' act = <price> ).

    ASSIGN COMPONENT 'FLAG' OF STRUCTURE <wa> TO FIELD-SYMBOL(<flag>).
    cl_abap_unit_assert=>assert_equals( exp = 'X' act = <flag> ).

  ENDMETHOD.

  METHOD create_data_false_and_null.

    DATA(lr_data) = build_data_by_json(
      json    = `{"flag": false, "note": null}`
      infer_types = abap_true ).

    ASSIGN lr_data->* TO FIELD-SYMBOL(<wa>).

    ASSIGN COMPONENT 'FLAG' OF STRUCTURE <wa> TO FIELD-SYMBOL(<flag>).
    cl_abap_unit_assert=>assert_initial( act = <flag> msg = 'false stays initial' ).

    ASSIGN COMPONENT 'NOTE' OF STRUCTURE <wa> TO FIELD-SYMBOL(<note>).
    cl_abap_unit_assert=>assert_initial( act = <note> msg = 'null stays initial' ).

  ENDMETHOD.

  METHOD create_data_array_root.

    DATA(lr_data) = build_data_by_json(
      json    = `[10, 20]`
      infer_types = abap_true ).

    ASSIGN lr_data->* TO FIELD-SYMBOL(<table>).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( <table> ) ).

    DATA(lv_index) = 1.
    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
      IF lv_index = 1.
        cl_abap_unit_assert=>assert_equals( exp = 10 act = <line> ).
      ELSE.
        cl_abap_unit_assert=>assert_equals( exp = 20 act = <line> ).
      ENDIF.
      lv_index = lv_index + 1.
    ENDLOOP.

  ENDMETHOD.

  METHOD create_data_invalid_json.

    cl_abap_unit_assert=>assert_equals(
      exp = 1 " invalid_json
      act = create_data_subrc( `{` )
      msg = 'invalid_json expected' ).

  ENDMETHOD.

  METHOD elem_c_length_diagnosis.

    " The RTTI length attribute is reported in different units on some
    " releases (bytes for character types). The authoritative check is
    " DESCRIBE FIELD ... IN CHARACTER MODE.
    DATA lr_data TYPE REF TO data.

    DATA(lo_elem) = cl_abap_elemdescr=>get_c( p_length = 1 ).
    CREATE DATA lr_data TYPE HANDLE lo_elem.
    ASSIGN lr_data->* TO FIELD-SYMBOL(<char>).
    DESCRIBE FIELD <char> LENGTH DATA(lv_chars) IN CHARACTER MODE.
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_chars
      msg = 'get_c(1) must produce a one character type' ).

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_field_tab(
      field_tab = VALUE zdot_datadescr(
                    ( fldname = 'FLAG' fldtype = 'F' intty = 'C' lengt = 1 ) )
      type      = 'S' ) ).

    DATA(lo_flag) = CAST cl_abap_elemdescr( component( struct = lo_struct name = 'FLAG' ) ).
    CREATE DATA lr_data TYPE HANDLE lo_flag.
    ASSIGN lr_data->* TO <char>.
    DESCRIBE FIELD <char> LENGTH lv_chars IN CHARACTER MODE.
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_chars
      msg = 'explicit C/1 field row must build a one character type' ).

  ENDMETHOD.

  METHOD name_map_shortens_long_key.

    " An over long JSON key is mapped to a valid ABAP component name
    " (mapping entries are matched case insensitively)
    DATA(lt_map) = VALUE zcl_dynamic_object=>ty_name_map(
      ( json = 'aVeryLongJsonKeyNameThatExceedsThirtyCharactersXyz' abap = 'short_name' ) ).

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json     = `{"aVeryLongJsonKeyNameThatExceedsThirtyCharactersXyz": 1}`
      infer_types = abap_true
      name_map = lt_map ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_int
      act = component( struct = lo_struct name = 'SHORT_NAME' )->type_kind
      msg = 'mapped component must exist with inferred type' ).

  ENDMETHOD.

  METHOD name_map_invalid_target_raises.

    " A mapped target that itself violates the ABAP name rules raises
    DATA(lt_map) = VALUE zcl_dynamic_object=>ty_name_map(
      ( json = 'X' abap = 'THIS_TARGET_NAME_IS_DEFINITELY_TOO_LONG_XYZ' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2 " invalid_field_name
      act = create_json_subrc( json = `{"x": 1}` name_map = lt_map )
      msg = 'invalid_field_name expected for over long mapping target' ).

  ENDMETHOD.

  METHOD name_map_dash_target_raises.

    DATA(lt_map) = VALUE zcl_dynamic_object=>ty_name_map(
      ( json = 'X' abap = 'A-B' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2 " invalid_field_name
      act = create_json_subrc( json = `{"x": 1}` name_map = lt_map )
      msg = 'invalid_field_name expected for mapping target with dash' ).

  ENDMETHOD.

  METHOD create_data_fills_mapped_name.

    DATA(lt_map) = VALUE zcl_dynamic_object=>ty_name_map(
      ( json = 'aVeryLongJsonKeyNameThatExceedsThirtyCharactersXyz' abap = 'short_name' ) ).

    DATA(lr_data) = build_data_by_json(
      json     = `{"aVeryLongJsonKeyNameThatExceedsThirtyCharactersXyz": "hi"}`
      name_map = lt_map ).

    ASSIGN lr_data->* TO FIELD-SYMBOL(<wa>).
    ASSIGN COMPONENT 'SHORT_NAME' OF STRUCTURE <wa> TO FIELD-SYMBOL(<mapped>).
    cl_abap_unit_assert=>assert_subrc( msg = 'mapped component missing' ).
    cl_abap_unit_assert=>assert_equals( exp = 'hi' act = <mapped> ).

  ENDMETHOD.

  METHOD walker_emits_typed_rows.

    " Diagnostic: inspect the raw rows the walker produces, before any
    " type building happens (used to pinpoint length mismatches)
    lcl_json_walker=>walk(
      EXPORTING
        json        = `{"flag": true, "price": 88.5}`
        infer_types = abap_true
      IMPORTING
        rows        = DATA(lt_rows) ).

    READ TABLE lt_rows WITH KEY fldname = 'FLAG' ASSIGNING FIELD-SYMBOL(<flag>).
    cl_abap_unit_assert=>assert_subrc( msg = 'FLAG row missing' ).
    cl_abap_unit_assert=>assert_equals( exp = 'F' act = <flag>-fldtype msg = 'flag fldtype' ).
    cl_abap_unit_assert=>assert_equals( exp = 'C' act = <flag>-intty msg = 'flag intty' ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = CONV i( <flag>-lengt ) msg = 'flag lengt must be 1' ).

    READ TABLE lt_rows WITH KEY fldname = 'PRICE' ASSIGNING FIELD-SYMBOL(<price>).
    cl_abap_unit_assert=>assert_subrc( msg = 'PRICE row missing' ).
    cl_abap_unit_assert=>assert_equals( exp = 'P' act = <price>-intty msg = 'price intty' ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = CONV i( <price>-lengt ) msg = 'price lengt must be 3' ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = CONV i( <price>-decim ) msg = 'price decim must be 1' ).

  ENDMETHOD.


  METHOD build_by_json.

    CALL METHOD zcl_dynamic_object=>create_by_json
      EXPORTING
        json_data            = json
        infer_types          = infer_types
        name_map             = name_map
      RECEIVING
        ref_type             = result
      EXCEPTIONS
        invalid_json         = 1
        invalid_field_name   = 2
        duplicate_components = 3
        execution_failed     = 4
        OTHERS               = 5.

    cl_abap_unit_assert=>assert_subrc( msg = |create_by_json failed for: { json }| ).

  ENDMETHOD.

  METHOD build_by_field_tab.

    CALL METHOD zcl_dynamic_object=>create_by_field_tab
      EXPORTING
        field_tab            = field_tab
        type                 = type
      RECEIVING
        ref_type             = result
      EXCEPTIONS
        unsupported_type     = 1
        execution_failed     = 2
        duplicate_components = 3
        OTHERS               = 4.

    cl_abap_unit_assert=>assert_subrc( msg = 'create_by_field_tab failed for field_tab' ).

  ENDMETHOD.

  METHOD build_data_by_json.

    CALL METHOD zcl_dynamic_object=>create_data_by_json
      EXPORTING
        json_data          = json
        infer_types        = infer_types
        name_map           = name_map
      RECEIVING
        ref_data           = result
      EXCEPTIONS
        invalid_json         = 1
        invalid_field_name   = 2
        duplicate_components = 3
        execution_failed     = 4
        OTHERS               = 5.

    cl_abap_unit_assert=>assert_subrc( msg = |create_data_by_json failed for: { json }| ).

  ENDMETHOD.

  METHOD create_data_subrc.

    CALL METHOD zcl_dynamic_object=>create_data_by_json
      EXPORTING
        json_data          = json
      RECEIVING
        ref_data           = DATA(lr_unused)
      EXCEPTIONS
        invalid_json         = 1
        invalid_field_name   = 2
        duplicate_components = 3
        execution_failed     = 4
        OTHERS               = 5.

    " sy-subrc must be captured before any statement (also asserts)
    " overwrites it
    result = sy-subrc.

    " On a raised classic exception the returning value stays initial
    cl_abap_unit_assert=>assert_initial( act = lr_unused ).

  ENDMETHOD.

  METHOD create_json_subrc.

    CALL METHOD zcl_dynamic_object=>create_by_json
      EXPORTING
        json_data            = json
        name_map             = name_map
      RECEIVING
        ref_type             = DATA(lr_unused)
      EXCEPTIONS
        invalid_json         = 1
        invalid_field_name   = 2
        duplicate_components = 3
        execution_failed     = 4
        OTHERS               = 5.

    " sy-subrc must be captured before any statement (also asserts)
    " overwrites it
    result = sy-subrc.

    " On a raised classic exception the returning value stays initial
    cl_abap_unit_assert=>assert_initial( act = lr_unused ).

  ENDMETHOD.

  METHOD create_field_tab_subrc.

    CALL METHOD zcl_dynamic_object=>create_by_field_tab
      EXPORTING
        field_tab            = field_tab
        type                 = type
      RECEIVING
        ref_type             = DATA(lr_unused)
      EXCEPTIONS
        unsupported_type     = 1
        execution_failed     = 2
        duplicate_components = 3
        OTHERS               = 4.

    " sy-subrc must be captured before any statement (also asserts)
    " overwrites it
    result = sy-subrc.

    " On a raised classic exception the returning value stays initial
    cl_abap_unit_assert=>assert_initial( act = lr_unused ).

  ENDMETHOD.

  METHOD component.

    DATA(lt_components) = struct->get_components( ).
    READ TABLE lt_components
      WITH KEY name = to_upper( name )
      ASSIGNING FIELD-SYMBOL(<comp>).
    cl_abap_unit_assert=>assert_subrc( msg = |component { name } not found| ).

    descr = <comp>-type.

  ENDMETHOD.

  METHOD table_line.

    line = CAST cl_abap_tabledescr( descr )->get_table_line_type( ).

  ENDMETHOD.

  METHOD assert_string_kind.

    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_string
      act = descr->type_kind ).

  ENDMETHOD.


  METHOD deep_nested_struct.

    " struct > struct > struct: type building must recurse two levels.
    " Guards the depth handling that the tree refactor (TODO #2) reworks;
    " on real systems this was never covered before (only one nesting
    " level existed in the suite).
    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      `{ "name": "Jack",`
      && ` "address": { "city": "Paris",`
      && `               "geo": { "lat": 1.5, "lng": 2.5 } } }` ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( lo_struct->get_components( ) ) ).

    DATA(lo_address) = CAST cl_abap_structdescr(
      component( struct = lo_struct name = 'ADDRESS' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lo_address->get_components( ) ) ).
    assert_string_kind( component( struct = lo_address name = 'CITY' ) ).

    DATA(lo_geo) = CAST cl_abap_structdescr(
      component( struct = lo_address name = 'GEO' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lo_geo->get_components( ) ) ).
    assert_string_kind( component( struct = lo_geo name = 'LAT' ) ).
    assert_string_kind( component( struct = lo_geo name = 'LNG' ) ).

  ENDMETHOD.


  METHOD deep_nested_table_in_table.

    " table > struct > table: array items containing a nested array
    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      `{ "skills": [ { "name": "ABAP",`
      && `               "tags": [ { "label": "core" } ] } ] }` ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( lo_struct->get_components( ) ) ).

    DATA(lo_skills_line) = CAST cl_abap_structdescr(
      table_line( component( struct = lo_struct name = 'SKILLS' ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = lines( lo_skills_line->get_components( ) ) ).
    assert_string_kind( component( struct = lo_skills_line name = 'NAME' ) ).

    DATA(lo_tags_line) = CAST cl_abap_structdescr(
      table_line( component( struct = lo_skills_line name = 'TAGS' ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lo_tags_line->get_components( ) ) ).
    assert_string_kind( component( struct = lo_tags_line name = 'LABEL' ) ).

  ENDMETHOD.


  METHOD create_data_deep_nested.

    " Values must land two levels deep (CREATE_DATA_BY_JSON fill path)
    DATA(lr_data) = build_data_by_json(
      `{ "company": "ACME",`
      && ` "headquarters": { "country": "FR",`
      && `   "location": { "city": "Paris" } } }` ).

    ASSIGN lr_data->* TO FIELD-SYMBOL(<wa>).
    ASSIGN COMPONENT 'HEADQUARTERS' OF STRUCTURE <wa> TO FIELD-SYMBOL(<hq>).
    ASSIGN COMPONENT 'LOCATION' OF STRUCTURE <hq> TO FIELD-SYMBOL(<loc>).
    ASSIGN COMPONENT 'CITY' OF STRUCTURE <loc> TO FIELD-SYMBOL(<city>).
    cl_abap_unit_assert=>assert_equals( exp = 'Paris' act = <city> ).

  ENDMETHOD.


  METHOD implicit_parent_becomes_struct.

    " Only 'A-B' is configured, no 'A' row: the undeclared intermediate
    " segment defaults to a structure instead of leaking 'B' to the top
    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_field_tab(
      field_tab = VALUE zdot_datadescr( ( fldname = 'A-B' fldtype = 'F' ) )
      type      = 'S' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( lo_struct->get_components( ) )
      msg = 'top level must be A only' ).

    DATA(lo_a) = CAST cl_abap_structdescr( component( struct = lo_struct name = 'A' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( lo_a->get_components( ) ) ).
    assert_string_kind( component( struct = lo_a name = 'B' ) ).

  ENDMETHOD.


  METHOD flat_pipeline_diagnosis.

    " Minimal direct builder call: reports the exact exception class
    " and text if the flat build fails (on-system diagnosis helper)
    TRY.
        DATA(lo_descr) = lcl_type_builder=>build(
          rows = VALUE zdot_datadescr(
                   ( fldname = 'ONE' fldtype = 'F' )
                   ( fldname = 'TWO' fldtype = 'F' ) )
          type = 'S' ).
        cl_abap_unit_assert=>assert_bound(
          act = lo_descr
          msg = 'flat build must produce a type' ).
      CATCH cx_dynamic_check INTO DATA(lx_error).
        DATA(lv_class) = cl_abap_classdescr=>describe_by_object_ref( lx_error )->absolute_name.
        cl_abap_unit_assert=>fail(
          msg = |flat build raised { lv_class }: { lx_error->get_text( ) }| ).
    ENDTRY.

  ENDMETHOD.


  METHOD field_tab_out_of_order.

    " Row order must not matter: children listed before their parent
    " and interleaved with other roots. Regression test for the tree
    " builder parent handling (the reverted 2.2.0 lr_parent leak)
    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_field_tab(
      field_tab = VALUE zdot_datadescr(
                    ( fldname = 'A-Y' fldtype = 'F' )
                    ( fldname = 'B' fldtype = 'F' )
                    ( fldname = 'A' fldtype = 'S' )
                    ( fldname = 'A-X' fldtype = 'F' ) )
      type      = 'S' ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( lo_struct->get_components( ) )
      msg = 'top level must be A and B' ).

    DATA(lo_a) = CAST cl_abap_structdescr( component( struct = lo_struct name = 'A' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( lo_a->get_components( ) )
      msg = 'A must keep both children' ).
    assert_string_kind( component( struct = lo_a name = 'X' ) ).
    assert_string_kind( component( struct = lo_a name = 'Y' ) ).
    assert_string_kind( component( struct = lo_struct name = 'B' ) ).

  ENDMETHOD.


  METHOD empty_struct_node_raises.

    " A struct row without children and without STRUF/INTTY is a field
    " description error: execution_failed (2) instead of an RTTS dump
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = create_field_tab_subrc(
              field_tab = VALUE zdot_datadescr( ( fldname = 'A' fldtype = 'S' ) )
              type      = 'S' )
      msg = 'empty struct node must raise execution_failed' ).

  ENDMETHOD.

ENDCLASS.
