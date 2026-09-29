CLASS zcl_dynamic_object DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_name_map_entry,
        json TYPE string,
        abap TYPE string,
      END OF ty_name_map_entry,
      ty_name_map TYPE SORTED TABLE OF ty_name_map_entry WITH UNIQUE KEY json .

    CONSTANTS:
      BEGIN OF c_info,
        version    TYPE string VALUE '3.1.0',
        author     TYPE string VALUE 'Jack Liang',
        email      TYPE string VALUE 'jack.liang.world@gmail.com',
        repository TYPE string VALUE 'https://github.com/Jack-Liang/DYNAMIC_DATA',
        license    TYPE string VALUE 'MIT',
      END OF c_info .

    CLASS-METHODS create_by_field_tab
      IMPORTING
        VALUE(field_tab) TYPE zdot_datadescr
        VALUE(type)      TYPE zdoe_fldtype
      RETURNING
        VALUE(ref_type)  TYPE REF TO cl_abap_datadescr
      EXCEPTIONS
        unsupported_type
        execution_failed
        duplicate_components .

    CLASS-METHODS create_by_json
      IMPORTING
        VALUE(json_data)  TYPE string
        VALUE(infer_types) TYPE abap_bool DEFAULT abap_false
        VALUE(name_map)   TYPE ty_name_map OPTIONAL
      RETURNING
        VALUE(ref_type)   TYPE REF TO cl_abap_datadescr
      EXCEPTIONS
        execution_failed
        duplicate_components
        invalid_json
        invalid_field_name .

    CLASS-METHODS create_data_by_json
      IMPORTING
        VALUE(json_data)  TYPE string
        VALUE(infer_types) TYPE abap_bool DEFAULT abap_false
        VALUE(name_map)   TYPE ty_name_map OPTIONAL
      RETURNING
        VALUE(ref_data)   TYPE REF TO data
      EXCEPTIONS
        execution_failed
        duplicate_components
        invalid_json
        invalid_field_name .

    CLASS-METHODS to_json
      IMPORTING
        VALUE(data)     TYPE REF TO data
        VALUE(name_map) TYPE ty_name_map OPTIONAL
      RETURNING
        VALUE(json)     TYPE string
      EXCEPTIONS
        unsupported_type .
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
        !type           TYPE zdoe_fldtype
      RETURNING
        VALUE(ref_type) TYPE REF TO cl_abap_datadescr
      EXCEPTIONS
        duplicate_components
        execution_failed .
    CLASS-METHODS normalize_name_map
      IMPORTING
        !name_map       TYPE ty_name_map
      RETURNING
        VALUE(result) TYPE ty_name_map .
ENDCLASS.



CLASS zcl_dynamic_object IMPLEMENTATION.


  METHOD create_by_field_tab.
*&----------------------------------------------------------------------------
*&    Create the type from a field description table.
*&    Project Address https://github.com/Jack-Liang/DYNAMIC_DATA (MIT)
*&    Author: Jack.Liang, Create Date: January 20, 2025
*&----------------------------------------------------------------------------

  IF type <> field_type-struct AND type <> field_type-table.
    RAISE unsupported_type.
  ENDIF.

  CALL METHOD build_type
    EXPORTING
      rows                 = field_tab
      root_is_array        = abap_false
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


  METHOD create_by_json.
*&----------------------------------------------------------------------------
*&    Create the type from JSON, parsed with the kernel sXML library
*&    (see the local classes in the LOCALS_IMP include).
*&----------------------------------------------------------------------------

  DATA lv_type TYPE zdoe_fldtype.
  DATA lt_rows TYPE zdot_datadescr.

  TRY.
      lcl_json_walker=>walk( EXPORTING json        = json_data
                                       infer_types = infer_types
                                       name_map    = normalize_name_map( name_map )
                             IMPORTING root_type      = lv_type
                                       root_is_array   = DATA(lv_root_is_array)
                                       rows            = lt_rows ).
    CATCH zcx_dynamic_json_error.
      RAISE invalid_json.
    CATCH zcx_dynamic_name_error.
      RAISE invalid_field_name.
  ENDTRY.

  CALL METHOD build_type
    EXPORTING
      rows                 = lt_rows
      root_is_array        = lv_root_is_array
      type                 = lv_type
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


METHOD create_data_by_json.
*&----------------------------------------------------------------------------
*&    One step JSON -> generated type + filled data object.
*&    The JSON is parsed once with the kernel sXML library, the type is
*&    generated from the parsed node table and the values are filled
*&    from the same node table. No JSON binder is needed on the caller
*&    side (booleans become 'X'/initial, null stays initial).
*&----------------------------------------------------------------------------

  DATA lt_nodes TYPE lcl_json_parser=>ty_nodes.
  DATA lt_rows TYPE zdot_datadescr.

  TRY.
      lt_nodes = lcl_json_parser=>parse( json_data ).

      DATA(lt_name_map) = normalize_name_map( name_map ).

      lcl_json_walker=>walk_nodes(
        EXPORTING
          nodes       = lt_nodes
          infer_types = infer_types
          name_map    = lt_name_map
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
      lcl_json_filler=>fill( nodes = lt_nodes data = ref_data name_map = lt_name_map ).
    CATCH zcx_dynamic_json_error.
      RAISE execution_failed.
  ENDTRY.

ENDMETHOD.


  METHOD to_json.
*&----------------------------------------------------------------------------
*&    Serialize a generated (or arbitrary) data object back to JSON -
*&    the inverse of CREATE_DATA_BY_JSON. Booleans (C length 1) become
*&    true/false, character-like initial values become null, numeric
*&    zeros are emitted as numbers, dates/times/hex are emitted as
*&    internal-format strings. NAME_MAP is applied in reverse
*&    (ABAP component name -> JSON key). An unbound reference
*&    serializes as null; reference components raise unsupported_type.
*&----------------------------------------------------------------------------

  IF data IS NOT BOUND.
    json = 'null'.
    RETURN.
  ENDIF.

  TRY.
      json = lcl_json_serializer=>serialize(
               data     = data
               name_map = normalize_name_map( name_map ) ).
    CATCH cx_dynamic_check.
      RAISE unsupported_type.
  ENDTRY.

  ENDMETHOD.


  METHOD build_type.

    "Duplicate field check, after upper case normalization
    "重复字段检查（在大写归一化之后）
    DATA lt_check TYPE zdot_datadescr.
    lt_check = rows.
    LOOP AT lt_check ASSIGNING FIELD-SYMBOL(<check>).
      TRANSLATE <check>-fldname TO UPPER CASE.
    ENDLOOP.
    SORT lt_check BY fldname.
    DELETE ADJACENT DUPLICATES FROM lt_check COMPARING fldname.
    IF lines( rows ) NE lines( lt_check ).
      RAISE duplicate_components.
    ENDIF.

    "An empty root JSON array still produces TABLE OF string
    "空数组作为根节点时仍然生成 TABLE OF string
    IF rows IS INITIAL AND root_is_array = abap_false.
      RAISE execution_failed.
    ENDIF.

    "CREATE TYPE: tree based builder in the LOCALS_IMP include.
    "The class stays stateless between calls - no static buffers.
    "创建类型：LOCALS_IMP 中的树形构建器，类在调用间无状态
    TRY.
        ref_type = lcl_type_builder=>build( rows = rows
                                            type = type ).
      CATCH cx_dynamic_check.
        RAISE execution_failed.
    ENDTRY.

    " like "XXXX": [ ]    OR   "XXX": ["XXX" ]
    " An empty table falls back to TABLE OF string
    IF ref_type IS NOT BOUND AND type = field_type-table.
      ref_type = cl_abap_tabledescr=>create( cl_abap_elemdescr=>get_string( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD normalize_name_map.

    " JSON keys are matched case insensitively after upper casing
    LOOP AT name_map ASSIGNING FIELD-SYMBOL(<map>).
      READ TABLE result TRANSPORTING NO FIELDS WITH KEY json = to_upper( <map>-json ).
      IF sy-subrc <> 0.
        INSERT VALUE #( json = to_upper( <map>-json )
                        abap = to_upper( <map>-abap ) ) INTO TABLE result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
