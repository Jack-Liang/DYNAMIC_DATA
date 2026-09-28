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
    METHODS empty_input_raises_unsupported FOR TESTING.

    METHODS build_by_json
      IMPORTING
        json    TYPE string
        no_type TYPE c DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_datadescr.

    METHODS build_by_field_tab
      IMPORTING
        field_tab TYPE zdot_datadescr
        type      TYPE zdoe_fldtype
      RETURNING
        VALUE(result) TYPE REF TO cl_abap_datadescr.

    METHODS create_json_subrc
      IMPORTING
        json TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS create_field_tab_subrc
      IMPORTING
        field_tab TYPE zdot_datadescr
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
      no_type = abap_false ) ).

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
      no_type = abap_false ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_int
      act = component( struct = lo_struct name = 'LEVEL' )->type_kind ).

  ENDMETHOD.

  METHOD packed_inferred.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"price": 88.5}`
      no_type = abap_false ) ).

    DATA(lo_elem) = CAST cl_abap_elemdescr( component( struct = lo_struct name = 'PRICE' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_packed
      act = lo_elem->type_kind ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_elem->decimals ).
    cl_abap_unit_assert=>assert_number_between(
      lower = 2
      upper = 16
      number = lo_elem->length
      msg = 'packed length must fit the literal digits' ).

  ENDMETHOD.

  METHOD bool_inferred.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"flag": true}`
      no_type = abap_false ) ).

    DATA(lo_elem) = CAST cl_abap_elemdescr( component( struct = lo_struct name = 'FLAG' ) ).
    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_char
      act = lo_elem->type_kind ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lo_elem->length ).

  ENDMETHOD.

  METHOD no_inference_by_default.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json( `{"n": 95}` ) ).

    assert_string_kind( component( struct = lo_struct name = 'N' ) ).

  ENDMETHOD.

  METHOD null_is_string.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"x": null}`
      no_type = abap_false ) ).

    cl_abap_unit_assert=>assert_bound( component( struct = lo_struct name = 'X' ) ).
    assert_string_kind( component( struct = lo_struct name = 'X' ) ).

  ENDMETHOD.

  METHOD array_of_scalars_typed.

    DATA(lo_struct) = CAST cl_abap_structdescr( build_by_json(
      json    = `{"points": [10, 20]}`
      no_type = abap_false ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = cl_abap_typedescr=>typekind_int
      act = table_line( component( struct = lo_struct name = 'POINTS' ) )->type_kind ).

  ENDMETHOD.

  METHOD union_of_array_items.

    " Different keys across items are unioned into the line structure
    DATA(lo_descr) = build_by_json(
      json    = `[{"a": 1}, {"b": "x"}]`
      no_type = abap_false ).

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
      exp = 4
      act = lv_subrc
      msg = 'invalid_json expected' ).

  ENDMETHOD.

  METHOD invalid_field_name_raises.

    DATA(lv_subrc) = create_json_subrc( `{"a b": 1}` ).

    cl_abap_unit_assert=>assert_equals(
      exp = 5
      act = lv_subrc
      msg = 'invalid_field_name expected' ).

  ENDMETHOD.

  METHOD dash_in_key_raises.

    " '-' is the hierarchy separator and cannot be part of a key
    DATA(lv_subrc) = create_json_subrc( `{"a-b": 1}` ).

    cl_abap_unit_assert=>assert_equals(
      exp = 5
      act = lv_subrc
      msg = 'invalid_field_name expected' ).

  ENDMETHOD.

  METHOD long_key_raises.

    DATA(lv_subrc) = create_json_subrc( `{"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA": 1}` ).

    cl_abap_unit_assert=>assert_equals(
      exp = 5
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

  METHOD empty_input_raises_unsupported.

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = create_json_subrc( `` )
      msg = 'unsupported_type expected' ).

  ENDMETHOD.


  METHOD build_by_json.

    CALL METHOD zcl_dynamic_object=>create_main
      EXPORTING
        json_data            = json
        no_type              = no_type
      RECEIVING
        ref_type             = result
      EXCEPTIONS
        unsupported_type     = 1
        execution_failed     = 2
        duplicate_components = 3
        invalid_json         = 4
        invalid_field_name   = 5
        OTHERS               = 6.

    cl_abap_unit_assert=>assert_subrc( msg = |create_main failed for: { json }| ).

  ENDMETHOD.

  METHOD build_by_field_tab.

    CALL METHOD zcl_dynamic_object=>create_main
      EXPORTING
        field_tab            = field_tab
        type                 = type
      RECEIVING
        ref_type             = result
      EXCEPTIONS
        unsupported_type     = 1
        execution_failed     = 2
        duplicate_components = 3
        invalid_json         = 4
        invalid_field_name   = 5
        OTHERS               = 6.

    cl_abap_unit_assert=>assert_subrc( msg = 'create_main failed for field_tab' ).

  ENDMETHOD.

  METHOD create_json_subrc.

    CALL METHOD zcl_dynamic_object=>create_main
      EXPORTING
        json_data            = json
      RECEIVING
        ref_type             = DATA(lr_unused)
      EXCEPTIONS
        unsupported_type     = 1
        execution_failed     = 2
        duplicate_components = 3
        invalid_json         = 4
        invalid_field_name   = 5
        OTHERS               = 6.

    " On a raised classic exception the returning value stays initial
    cl_abap_unit_assert=>assert_initial( act = lr_unused ).
    result = sy-subrc.

  ENDMETHOD.

  METHOD create_field_tab_subrc.

    CALL METHOD zcl_dynamic_object=>create_main
      EXPORTING
        field_tab            = field_tab
        type                 = 'S'
      RECEIVING
        ref_type             = DATA(lr_unused)
      EXCEPTIONS
        unsupported_type     = 1
        execution_failed     = 2
        duplicate_components = 3
        invalid_json         = 4
        invalid_field_name   = 5
        OTHERS               = 6.

    " On a raised classic exception the returning value stays initial
    cl_abap_unit_assert=>assert_initial( act = lr_unused ).
    result = sy-subrc.

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

ENDCLASS.
