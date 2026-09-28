CLASS zcl_dynamic_object DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_split TYPE TABLE OF string .

    CONSTANTS:
      BEGIN OF c_info,
        version    TYPE string VALUE '2.2.0',
        author     TYPE string VALUE 'Jack Liang',
        email      TYPE string VALUE 'jack.liang.world@gmail.com',
        repository TYPE string VALUE 'https://github.com/Jack-Liang/DYNAMIC_DATA',
        license    TYPE string VALUE 'MIT',
      END OF c_info .

    CLASS-METHODS create_main
      IMPORTING
        VALUE(field_tab) TYPE zdot_datadescr OPTIONAL
        VALUE(type)      TYPE zdoe_fldtype OPTIONAL
        VALUE(json_data) TYPE string OPTIONAL
        VALUE(no_type)   TYPE c DEFAULT abap_true
      RETURNING
        VALUE(ref_type)  TYPE REF TO cl_abap_datadescr
      EXCEPTIONS
        unsupported_type
        execution_failed
        duplicate_components
        invalid_json
        invalid_field_name .

    CLASS-METHODS create_data
      IMPORTING
        VALUE(json_data) TYPE string
        VALUE(no_type)   TYPE c DEFAULT abap_true
      RETURNING
        VALUE(ref_data)  TYPE REF TO data
      EXCEPTIONS
        unsupported_type
        execution_failed
        duplicate_components
        invalid_json
        invalid_field_name .
  PROTECTED SECTION.


  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF field_type,
        field  TYPE char1  VALUE 'F',
        struct TYPE char1  VALUE 'S',
        table  TYPE char1  VALUE 'T',
      END OF field_type .

    CLASS-METHODS build_type
      IMPORTING
        !rows           TYPE zdot_datadescr
        !root_is_array  TYPE abap_bool
      CHANGING
        !type           TYPE zdoe_fldtype
      RETURNING
        VALUE(ref_type) TYPE REF TO cl_abap_datadescr
      EXCEPTIONS
        duplicate_components
        execution_failed .
ENDCLASS.



CLASS zcl_dynamic_object IMPLEMENTATION.


METHOD create_main.
*&----------------------------------------------------------------------------
*&
*&    Desecription:
*&        This repository is mainly used for dynamically creating nested data types within the program
*&        Project Address https://github.com/Jack-Liang/DYNAMIC_DATA
*&        Please abide by the  MIT license of this project
*&        Welcome to provide new features to this repository
*&    Author         :   Jack.Liang
*&    Create Date:   January 20, 2025
*&    Program contact:   jack.liang.world@gmail.com
*&----------------------------------------------------------------------------
*&    Overview
*&
*&----------------------------------------------------------------------------
*&    Change List
*&    change NO.    Change Date    Change User    Change Detail
*&
*&----------------------------------------------------------------------------

  DATA lt_rows TYPE zdot_datadescr.

  "Entry check
  "入参检查
  IF json_data IS INITIAL
    AND ( type <> field_type-struct AND type <> field_type-table ).
    RAISE unsupported_type.
  ENDIF.

  IF json_data IS NOT INITIAL.
    " JSON -> field description table, parsed with the kernel sXML library
    " (see the local classes in the LOCALS_IMP include)
    TRY.
        lcl_json_walker=>walk( EXPORTING json        = json_data
                                         infer_types = boolc( no_type <> abap_true )
                               IMPORTING root_type      = type
                                         root_is_array   = DATA(lv_root_is_array)
                                         rows            = lt_rows ).
      CATCH zcx_dynamic_json_error.
        RAISE invalid_json.
      CATCH zcx_dynamic_name_error.
        RAISE invalid_field_name.
    ENDTRY.
  ELSE.
    lt_rows = field_tab.
  ENDIF.

  "CREATE TYPE
  "创建类型
  CALL METHOD build_type
    EXPORTING
      rows                 = lt_rows
      root_is_array        = lv_root_is_array
    CHANGING
      type                 = type
    RECEIVING
      ref_type             = ref_type
    EXCEPTIONS
      duplicate_components = 1
      execution_failed     = 2
      OTHERS               = 3.

  IF sy-subrc = 1.
    RAISE duplicate_components.
  ELSEIF sy-subrc <> 0.
    RAISE execution_failed.
  ENDIF.

ENDMETHOD.


METHOD create_data.
*&----------------------------------------------------------------------------
*&    One step JSON -> generated type + filled data object.
*&    The JSON is parsed once with the kernel sXML library, the type is
*&    generated from the parsed node table and the values are filled
*&    from the same node table. No JSON binder is needed on the caller
*&    side (booleans become 'X'/initial, null stays initial).
*&----------------------------------------------------------------------------

  DATA lt_nodes TYPE lcl_json_parser=>ty_nodes.
  DATA lt_rows TYPE zdot_datadescr.

  IF json_data IS INITIAL.
    RAISE unsupported_type.
  ENDIF.

  TRY.
      lt_nodes = lcl_json_parser=>parse( json_data ).

      lcl_json_walker=>walk_nodes(
        EXPORTING
          nodes       = lt_nodes
          infer_types = boolc( no_type <> abap_true )
        IMPORTING
          root_type     = DATA(lv_type)
          root_is_array = DATA(lv_root_is_array)
          rows          = lt_rows ).
    CATCH zcx_dynamic_json_error.
      RAISE invalid_json.
    CATCH zcx_dynamic_name_error.
      RAISE invalid_field_name.
  ENDTRY.

  CALL METHOD build_type
    EXPORTING
      rows                 = lt_rows
      root_is_array        = lv_root_is_array
    CHANGING
      type                 = lv_type
    RECEIVING
      ref_type             = DATA(lr_type)
    EXCEPTIONS
      duplicate_components = 1
      execution_failed     = 2
      OTHERS               = 3.

  IF sy-subrc = 1.
    RAISE duplicate_components.
  ELSEIF sy-subrc <> 0 OR lr_type IS NOT BOUND.
    RAISE execution_failed.
  ENDIF.

  CREATE DATA ref_data TYPE HANDLE lr_type.

  TRY.
      lcl_json_filler=>fill( nodes = lt_nodes data = ref_data ).
    CATCH zcx_dynamic_json_error.
      RAISE execution_failed.
  ENDTRY.

ENDMETHOD.


  METHOD build_type.

    "Normalize all field names to upper case
    "把所有字段名转为大写
    DATA lt_rows TYPE zdot_datadescr.
    lt_rows = rows.
    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row>).
      <row>-fldname = to_upper( <row>-fldname ).
    ENDLOOP.

    "Duplicate field check
    "重复字段检查
    DATA(lt_check) = lt_rows.
    SORT lt_check BY fldname.
    DELETE ADJACENT DUPLICATES FROM lt_check COMPARING fldname.
    IF lines( lt_rows ) NE lines( lt_check ).
      RAISE duplicate_components.
    ENDIF.

    "An empty root JSON array still produces TABLE OF string
    "空数组作为根节点时仍然生成 TABLE OF string
    IF lt_rows IS INITIAL AND root_is_array = abap_false.
      RAISE execution_failed.
    ENDIF.

    "CREATE TYPE: build a tree first, then generate the types bottom up
    "(see LCL_TYPE_BUILDER in the LOCALS_IMP include)
    "创建类型：先构树，再自底向上生成
    TRY.
        ref_type = lcl_type_builder=>build( rows = lt_rows type = type ).
      CATCH cx_dynamic_check.
        RAISE execution_failed.
    ENDTRY.

    "  like "XXXX": [ ]    OR   "XXX": ["XXX" ]
    " An empty table falls back to TABLE OF string
    IF ref_type IS NOT BOUND AND type = field_type-table.
      ref_type = cl_abap_tabledescr=>create( cl_abap_elemdescr=>get_string( ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
