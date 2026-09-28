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
        version    TYPE string VALUE '3.0.0',
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
  PROTECTED SECTION.


  PRIVATE SECTION.

    TYPES:
      ty_split TYPE TABLE OF string .

    CLASS-DATA gt_field_tab TYPE zdot_datadescr .
    CONSTANTS:
      BEGIN OF field_type,
        field  TYPE char1  VALUE 'F',
        struct TYPE char1  VALUE 'S',
        table  TYPE char1  VALUE 'T',
      END OF field_type .
    CONSTANTS:
      BEGIN OF c_des_methd,
        by_data       TYPE string VALUE 'DESCRIBE_BY_DATA',
        by_name       TYPE string VALUE 'DESCRIBE_BY_NAME',
        by_object_ref TYPE string VALUE 'DESCRIBE_BY_OBJECT_REF',
        by_data_ref   TYPE string VALUE 'DESCRIBE_BY_DATA_REF',
      END OF c_des_methd .

    CLASS-METHODS build_from_rows
      IMPORTING
        VALUE(type)     TYPE zdoe_fldtype
      EXPORTING
        VALUE(ref_type) TYPE REF TO cl_abap_datadescr
        VALUE(ref_data) TYPE REF TO data
      CHANGING
        !field_tab      TYPE zdot_datadescr .
    CLASS-METHODS append_field
      IMPORTING
        !fldname  TYPE data
        !method   TYPE string
        !object   TYPE data
      CHANGING
        !comp_tab TYPE abap_component_tab .
    CLASS-METHODS build_elem_descr
      IMPORTING
        !intty TYPE inttype
        !lengt TYPE ilen
        !decim TYPE decimals
      RETURNING
        VALUE(descr) TYPE REF TO cl_abap_datadescr .
    CLASS-METHODS build_type
      IMPORTING
        !root_is_array TYPE abap_bool
      CHANGING
        !type          TYPE zdoe_fldtype
      RETURNING
        VALUE(ref_type) TYPE REF TO cl_abap_datadescr
      EXCEPTIONS
        duplicate_components
        execution_failed .
    CLASS-METHODS structural_sub
      CHANGING
        !field_tab TYPE zdot_datadescr
        !split_tab TYPE ty_split .
    CLASS-METHODS put_parent_field_first .
    CLASS-METHODS normalize_name_map
      IMPORTING
        !name_map       TYPE ty_name_map
      RETURNING
        VALUE(result) TYPE ty_name_map .
ENDCLASS.



CLASS zcl_dynamic_object IMPLEMENTATION.


  METHOD append_field.

    DATA ls_comp TYPE LINE OF abap_component_tab.

    ls_comp-name = fldname.
    CASE method.
      WHEN c_des_methd-by_data.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_data( object ).
      WHEN c_des_methd-by_name.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_name( object ).
      WHEN c_des_methd-by_object_ref.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_object_ref( object ).
      WHEN c_des_methd-by_data_ref.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_data_ref( object ).
    ENDCASE.

    APPEND ls_comp TO comp_tab.


  ENDMETHOD.


  METHOD build_elem_descr.

    " Returns a type descriptor for an elementary type specification
    " (INTTY/LENGT/DECIM of the field description table).
    " LENGT/DECIM are DDIC text-like fields and need explicit conversion
    " for the numeric RTTS factory parameters.
    DATA(lv_length) = CONV i( lengt ).
    DATA(lv_decimals) = CONV i( decim ).

    CASE intty.
      WHEN 'P'.
        descr = cl_abap_elemdescr=>get_p( p_length = lv_length p_decimals = lv_decimals ).
      WHEN 'C'.
        descr = cl_abap_elemdescr=>get_c( p_length = lv_length ).
      WHEN 'N'.
        descr = cl_abap_elemdescr=>get_n( p_length = lv_length ).
      WHEN 'X'.
        descr = cl_abap_elemdescr=>get_x( p_length = lv_length ).
      WHEN 'g'.
        descr = cl_abap_elemdescr=>get_string( ).
      WHEN 'I'.
        descr = cl_abap_elemdescr=>get_i( ).
      WHEN 'F'.
        descr = cl_abap_elemdescr=>get_f( ).
      WHEN 'D'.
        descr = cl_abap_elemdescr=>get_d( ).
      WHEN 'T'.
        descr = cl_abap_elemdescr=>get_t( ).
      WHEN OTHERS.
        descr ?= cl_abap_typedescr=>describe_by_name( intty ).
    ENDCASE.

  ENDMETHOD.


  METHOD build_from_rows.

    DATA lt_datadescr TYPE zdot_datadescr.
    DATA lt_split TYPE TABLE OF string.

    DATA lt_comp     TYPE abap_component_tab.
    DATA lr_struc    TYPE REF TO cl_abap_structdescr.
    DATA lr_table    TYPE REF TO cl_abap_tabledescr.

    DATA l_dyn_obj TYPE REF TO data.

    LOOP AT field_tab ASSIGNING FIELD-SYMBOL(<fs_datadescr>)
      WHERE flag = abap_false.

      FREE l_dyn_obj.

      <fs_datadescr>-flag = abap_true.

      SPLIT <fs_datadescr>-fldname AT '-' INTO TABLE lt_split.

      DATA(lines) = lines( lt_split ).
      READ TABLE lt_split ASSIGNING FIELD-SYMBOL(<fs_split>) INDEX lines.

      CASE <fs_datadescr>-fldtype.
        WHEN field_type-field.

          IF <fs_datadescr>-struf IS NOT INITIAL.
            append_field( EXPORTING fldname  = <fs_split>
                                    method   = c_des_methd-by_name
                                    object   = <fs_datadescr>-struf
                          CHANGING  comp_tab = lt_comp[] ).
          ELSEIF <fs_datadescr>-intty IS NOT INITIAL.
            " Elementary type specification (INTTY/LENGT/DECIM): build the
            " descriptor directly via RTTS factories. CREATE DATA with the
            " text-like DDIC fields in LENGTH/DECIMALS is unreliable.
            APPEND VALUE #( name = <fs_split>
                            type = build_elem_descr( intty = <fs_datadescr>-intty
                                                     lengt = <fs_datadescr>-lengt
                                                     decim = <fs_datadescr>-decim ) ) TO lt_comp.

          ELSEIF <fs_datadescr>-refty IS BOUND.
            append_field( EXPORTING fldname  = <fs_split>
                                    method   = c_des_methd-by_data_ref
                                    object   = <fs_datadescr>-refty
                          CHANGING  comp_tab = lt_comp[] ).
          ELSE.
            append_field( EXPORTING fldname  = <fs_split>
                                    method   = c_des_methd-by_name
                                    object   = 'STRING'
                          CHANGING  comp_tab = lt_comp[] ).
          ENDIF.

        WHEN field_type-struct OR field_type-table.

          " 以上两种类型，不支持参考基本类型 The preceding two types do not support reference basic types
          " 如果设置了参考对象，不支持设置下级字段 If the reference object is set, the setting of subordinate fields is not supported
          "构造下级字段列表 Constructs a list of subordinate fields
          IF <fs_datadescr>-struf IS NOT INITIAL.

            IF <fs_datadescr>-fldtype = field_type-table.
              CREATE DATA l_dyn_obj TYPE TABLE OF (<fs_datadescr>-struf).
              append_field( EXPORTING fldname  = <fs_split>
                                      method   = c_des_methd-by_data_ref
                                      object   = l_dyn_obj
                            CHANGING  comp_tab = lt_comp[] ).
            ELSE.
              append_field( EXPORTING fldname  = <fs_split>
                                      method   = c_des_methd-by_name
                                      object   = <fs_datadescr>-struf
                            CHANGING  comp_tab = lt_comp[] ).
            ENDIF.

          ELSEIF <fs_datadescr>-intty IS NOT INITIAL.
            " 表类型但字段为基本类型（如 json 数组的元素类型推断）
            " Table whose line is an elementary type (e.g. inferred from a json array)
            lr_table = cl_abap_tabledescr=>create(
              build_elem_descr( intty = <fs_datadescr>-intty
                                lengt = <fs_datadescr>-lengt
                                decim = <fs_datadescr>-decim ) ).

            CREATE DATA l_dyn_obj TYPE HANDLE lr_table.

            append_field( EXPORTING fldname  = <fs_split>
                                    method   = c_des_methd-by_data_ref
                                    object   = l_dyn_obj
                          CHANGING  comp_tab = lt_comp[] ).

          ELSE.

            structural_sub( CHANGING field_tab = lt_datadescr split_tab = lt_split ).

            build_from_rows( EXPORTING type      = <fs_datadescr>-fldtype
                             IMPORTING ref_data  = l_dyn_obj         "创建一个类型 Create a type
                             CHANGING  field_tab = lt_datadescr ).

            append_field( EXPORTING fldname  = <fs_split>
                                    method   = c_des_methd-by_data_ref
                                    object   = l_dyn_obj "创建好的类型 Created type
                          CHANGING  comp_tab = lt_comp[] ).
          ENDIF.

      ENDCASE.

    ENDLOOP.


    IF lt_comp IS NOT INITIAL.

      lr_struc = cl_abap_structdescr=>create( lt_comp ).

      IF type = field_type-struct.
        CREATE DATA ref_data  TYPE HANDLE lr_struc.
        ref_type ?= lr_struc.
      ELSEIF type = field_type-table.
        lr_table = cl_abap_tabledescr=>create( lr_struc ).
        CREATE DATA ref_data  TYPE HANDLE lr_table.
        ref_type ?= lr_table.
      ENDIF.

    ENDIF.

    "  like "XXXX": [ ]    OR   "XXX": ["XXX" ]
    " An empty table falls back to TABLE OF string
    IF ref_data IS INITIAL AND type = field_type-table.
      lr_table = cl_abap_tabledescr=>create( cl_abap_elemdescr=>get_string( ) ).
      CREATE DATA ref_data TYPE HANDLE lr_table.
      ref_type ?= lr_table.
    ENDIF.


  ENDMETHOD.


  METHOD create_by_field_tab.
*&----------------------------------------------------------------------------
*&    Create the type from a field description table.
*&    Project Address https://github.com/Jack-Liang/DYNAMIC_DATA (MIT)
*&    Author: Jack.Liang, Create Date: January 20, 2025
*&----------------------------------------------------------------------------

  IF type <> field_type-struct AND type <> field_type-table.
    RAISE unsupported_type.
  ENDIF.

  gt_field_tab = field_tab.

  CALL METHOD build_type
    EXPORTING
      root_is_array        = abap_false
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


  METHOD create_by_json.
*&----------------------------------------------------------------------------
*&    Create the type from JSON, parsed with the kernel sXML library
*&    (see the local classes in the LOCALS_IMP include).
*&----------------------------------------------------------------------------

  DATA lv_type TYPE zdoe_fldtype.

  CLEAR: gt_field_tab.

  TRY.
      lcl_json_walker=>walk( EXPORTING json        = json_data
                                       infer_types = infer_types
                                       name_map    = normalize_name_map( name_map )
                             IMPORTING root_type      = lv_type
                                       root_is_array   = DATA(lv_root_is_array)
                                       rows            = gt_field_tab ).
    CATCH zcx_dynamic_json_error.
      RAISE invalid_json.
    CATCH zcx_dynamic_name_error.
      RAISE invalid_field_name.
  ENDTRY.

  CALL METHOD build_type
    EXPORTING
      root_is_array        = lv_root_is_array
    CHANGING
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

  CLEAR: gt_field_tab.

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
          rows          = gt_field_tab ).
    CATCH zcx_dynamic_json_error.
      RAISE invalid_json.
    CATCH zcx_dynamic_name_error.
      RAISE invalid_field_name.
  ENDTRY.

  CALL METHOD build_type
    EXPORTING
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
      lcl_json_filler=>fill( nodes = lt_nodes data = ref_data name_map = lt_name_map ).
    CATCH zcx_dynamic_json_error.
      RAISE execution_failed.
  ENDTRY.

ENDMETHOD.


  METHOD build_type.

    "Put the parent field first (also translates all names to upper case)
    "把上级字段放在前面（同时把所有字段名转为大写）
    put_parent_field_first( ).

    "Duplicate field check, after upper case normalization
    "重复字段检查（在大写归一化之后）
    DATA(lt_check) = gt_field_tab.
    SORT lt_check BY fldname.
    DELETE ADJACENT DUPLICATES FROM lt_check COMPARING fldname.
    IF lines( gt_field_tab ) NE lines( lt_check ).
      RAISE duplicate_components.
    ENDIF.

    "An empty root JSON array still produces TABLE OF string
    "空数组作为根节点时仍然生成 TABLE OF string
    IF gt_field_tab IS INITIAL AND root_is_array = abap_false.
      RAISE execution_failed.
    ENDIF.

    "CREATE TYPE
    "创建类型
    build_from_rows( EXPORTING type      = type
                     IMPORTING ref_type  = ref_type
                     CHANGING  field_tab = gt_field_tab ).

  ENDMETHOD.


  METHOD structural_sub.

    DATA lt_split TYPE TABLE OF string.
    DATA ls_datadescr TYPE zdos_datadescr.

    "计算层级 ( 删除由于提层导致的差异 )
    "Calculate the depth (remove the difference caused by the level)
    DATA lt_callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = lt_callstack.

    DELETE lt_callstack WHERE blockname <> 'BUILD_FROM_ROWS'.
    DATA(lv_depth) = lines( lt_callstack ) - 1.

    LOOP AT gt_field_tab ASSIGNING FIELD-SYMBOL(<fs_datadescr>)
      WHERE flag = abap_false.

      SPLIT <fs_datadescr>-fldname AT '-' INTO TABLE lt_split.

      IF lv_depth > 0.
        DELETE lt_split FROM 1 TO lv_depth.
      ENDIF.

      "判断是否上下级关系
      "Check the parent-child relationship
      IF lines( split_tab ) + 1 = lines( lt_split ).

        READ TABLE lt_split INDEX lines( lt_split ) ASSIGNING FIELD-SYMBOL(<fs_split>).
        DATA(lv_field) = <fs_split>.
        DELETE lt_split INDEX lines( lt_split ).
        IF split_tab = lt_split."上级相同，确定为下级字段

          MOVE-CORRESPONDING <fs_datadescr> TO ls_datadescr.
          ls_datadescr-fldname = lv_field.
          APPEND ls_datadescr TO field_tab.

          <fs_datadescr>-flag = abap_true.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD put_parent_field_first.

    IF gt_field_tab IS INITIAL.
      RETURN.
    ENDIF.

    DATA lt_field LIKE gt_field_tab.
    DATA ls_field LIKE LINE OF gt_field_tab.
    DATA lt_parts TYPE TABLE OF string.
    DATA lv_field TYPE string.

    CONSTANTS lc_sep TYPE c VALUE '-'.

    LOOP AT gt_field_tab ASSIGNING FIELD-SYMBOL(<fs_field>).
      TRANSLATE <fs_field>-fldname TO UPPER CASE.
    ENDLOOP.

    LOOP AT gt_field_tab INTO ls_field.

      IF ls_field-fldname CS lc_sep.
        CLEAR: lv_field, lt_parts.

        SPLIT ls_field-fldname AT lc_sep INTO TABLE lt_parts.
        DELETE lt_parts INDEX lines( lt_parts ).

        CONCATENATE LINES OF lt_parts INTO lv_field SEPARATED BY lc_sep.

        READ TABLE lt_field WITH KEY fldname = lv_field TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          READ TABLE gt_field_tab INTO DATA(ls_row) WITH KEY fldname = lv_field.
          IF sy-subrc = 0.
            APPEND ls_row TO lt_field.
            DELETE TABLE gt_field_tab FROM ls_row.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ls_field TO lt_field.

    ENDLOOP.

    gt_field_tab = lt_field.

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
