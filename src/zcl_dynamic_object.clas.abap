CLASS zcl_dynamic_object DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_split TYPE TABLE OF string .

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
        duplicate_components .
  PROTECTED SECTION.


  PRIVATE SECTION.

    CLASS-DATA global_field_tab TYPE zdot_datadescr .
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
    CLASS-DATA no_json_type TYPE c .

    CLASS-METHODS create_by_field_tab
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
    CLASS-METHODS structural_sub
      CHANGING
        !field_tab TYPE zdot_datadescr
        !split_tab TYPE ty_split .
    CLASS-METHODS deserialize_to_field_tab
      IMPORTING
        !json_data  TYPE string
      CHANGING
        VALUE(type) TYPE zdoe_fldtype .
    CLASS-METHODS check_object
      IMPORTING
        !i_abap_type    TYPE REF TO cl_abap_structdescr
        VALUE(i_data)   TYPE REF TO data
        VALUE(i_parent) TYPE string .
    CLASS-METHODS check_component
      IMPORTING
        VALUE(i_parent) TYPE string
        !i_comp         TYPE abap_compdescr
        VALUE(i_data)   TYPE REF TO data .
    CLASS-METHODS put_parent_field_first .
ENDCLASS.



CLASS ZCL_DYNAMIC_OBJECT IMPLEMENTATION.


  METHOD append_field.

    DATA ls_comp TYPE LINE OF abap_component_tab.

    ls_comp-name = fldname.
    CASE method.
      WHEN c_des_methd-by_data.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_data( object ).
      WHEN c_des_methd-by_name.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_name( object ).
      WHEN c_des_methd-by_object_ref .
        ls_comp-type ?= cl_abap_typedescr=>describe_by_object_ref( object ).
      WHEN c_des_methd-by_data_ref.
        ls_comp-type ?= cl_abap_typedescr=>describe_by_data_ref( object ).
      WHEN OTHERS.
    ENDCASE.

    APPEND ls_comp TO comp_tab.


  ENDMETHOD.


  METHOD create_by_field_tab.

    DATA lv_str TYPE string.

    DATA lt_datadescr TYPE zdot_datadescr.
    DATA ls_datadescr TYPE zdos_datadescr.
    DATA lt_split TYPE TABLE OF string.
    DATA tmp_split LIKE lt_split.

    DATA lt_comp    TYPE abap_component_tab.
    DATA lr_struc     TYPE REF TO cl_abap_structdescr.
    DATA lr_table     TYPE REF TO cl_abap_tabledescr.

    DATA l_dyn_s  TYPE REF TO cl_abap_datadescr.
    DATA l_dyn_obj  TYPE REF TO data.

    LOOP AT field_tab ASSIGNING FIELD-SYMBOL(<fs_datadescr>)
      WHERE flag = abap_false.

      FREE l_dyn_obj.

      <fs_datadescr>-flag = abap_true.

      SPLIT <fs_datadescr>-fldname AT '-' INTO TABLE lt_split.


      DESCRIBE TABLE lt_split LINES DATA(lines).
      READ TABLE lt_split ASSIGNING FIELD-SYMBOL(<fs_split>) INDEX lines.

      CASE <fs_datadescr>-fldtype.
        WHEN field_type-field.

          IF <fs_datadescr>-struf IS NOT INITIAL.
            append_field( EXPORTING fldname  = <fs_split>
                                    method   = c_des_methd-by_name
                                    object   = <fs_datadescr>-struf
                          CHANGING  comp_tab = lt_comp[] ).
          ELSEIF <fs_datadescr>-intty IS NOT INITIAL.
            "创建一个本地类型 作为参考 Create a local type for reference
            CASE <fs_datadescr>-intty.
              WHEN 'P'.
                CREATE DATA l_dyn_obj TYPE p LENGTH <fs_datadescr>-lengt DECIMALS <fs_datadescr>-decim.
              WHEN 'C' OR 'N' OR 'X' .
                CREATE DATA l_dyn_obj TYPE (<fs_datadescr>-intty) LENGTH <fs_datadescr>-lengt .
              WHEN 'g'.
                CREATE DATA l_dyn_obj TYPE string.
              WHEN OTHERS.
                CREATE DATA l_dyn_obj TYPE (<fs_datadescr>-intty)  .
            ENDCASE.

            append_field( EXPORTING fldname  = <fs_split>
                                    method   = c_des_methd-by_data_ref
                                    object   = l_dyn_obj
                          CHANGING  comp_tab = lt_comp[] ).

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
              CREATE DATA l_dyn_obj TYPE TABLE OF (<fs_datadescr>-struf) .
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

          ELSE.

            structural_sub( CHANGING field_tab = lt_datadescr split_tab = lt_split ).

            create_by_field_tab( EXPORTING type      = <fs_datadescr>-fldtype
                                 IMPORTING ref_data  = l_dyn_obj         "创建一个类型 Create a type
                                 CHANGING  field_tab = lt_datadescr ).

            append_field( EXPORTING fldname  = <fs_split>
                                    method   = c_des_methd-by_data_ref
                                    object   = l_dyn_obj "创建好的类型 Created type
                          CHANGING  comp_tab = lt_comp[] ).
          ENDIF.

        WHEN OTHERS.

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
    IF ref_data IS INITIAL AND type = field_type-table.
      CREATE DATA ref_data TYPE TABLE OF string.
    ENDIF.


  ENDMETHOD.


  METHOD create_main.

    CLEAR: global_field_tab.

    "Entry check
    "入参检查
    IF json_data IS INITIAL
      AND ( type <> field_type-struct AND type <> field_type-table ).
      RAISE unsupported_type.
    ENDIF.

    IF json_data IS NOT INITIAL.
      no_json_type = no_type.
      deserialize_to_field_tab( EXPORTING json_data = json_data CHANGING type = type ).
    ELSE.
      global_field_tab[] = field_tab[].
    ENDIF.

    IF global_field_tab[] IS INITIAL.
      RAISE execution_failed.
    ENDIF.

    "Duplicate field check
    "重复字段检查
    DATA(lt_field_tab) = global_field_tab.

    SORT lt_field_tab BY fldname.
    DELETE ADJACENT DUPLICATES FROM lt_field_tab COMPARING fldname.
    IF lines( global_field_tab ) NE lines( lt_field_tab ).
      RAISE duplicate_components.
    ENDIF.
    FREE lt_field_tab.

    "Put the parent field first
    "把上级字段放在前面
    put_parent_field_first( ).

    "CREATE TYPE
    "创建类型
    create_by_field_tab( EXPORTING type      = type
                         IMPORTING ref_type  = ref_type
                         CHANGING  field_tab = global_field_tab ).



  ENDMETHOD.


  METHOD structural_sub.

    DATA lt_split TYPE TABLE OF string.
    DATA ls_datadescr TYPE zdos_datadescr.

    "计算层级 ( 删除由于提层导致的差异 )
    DATA mt_callstack TYPE abap_callstack .
    FIELD-SYMBOLS: <ls_callstack> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = mt_callstack.

    DELETE mt_callstack WHERE blockname NE 'CREATE_BY_FIELD_TAB'.
    DESCRIBE TABLE mt_callstack LINES DATA(i).
    i = i - 1.

    LOOP AT global_field_tab ASSIGNING FIELD-SYMBOL(<fs_datadescr>)
      WHERE flag = abap_false.

      SPLIT <fs_datadescr>-fldname AT '-' INTO TABLE lt_split.

      IF i GT 0.
        DELETE lt_split FROM 1 TO i.
      ENDIF.

      "判断是否上下级关系
      IF lines( split_tab ) + 1 = lines( lt_split ).

        READ TABLE lt_split INDEX lines( lt_split ) ASSIGNING FIELD-SYMBOL(<fs_split>) .
        DATA(lv_filed) = <fs_split>.
        DELETE lt_split INDEX lines( lt_split ).
        IF split_tab = lt_split."上级相同，确定为下级字段

          MOVE-CORRESPONDING <fs_datadescr> TO ls_datadescr.
          ls_datadescr-fldname = lv_filed.
          APPEND ls_datadescr TO field_tab.

          <fs_datadescr>-flag = abap_true.
        ENDIF.

      ENDIF.
    ENDLOOP.



  ENDMETHOD.


  METHOD check_component.

    DATA  lv_parent TYPE string.
    FIELD-SYMBOLS: <tab>  TYPE STANDARD TABLE,
                   <test> TYPE any.
    DATA: abap_type TYPE REF TO cl_abap_structdescr.

    IF i_parent IS INITIAL.
      lv_parent = i_comp-name.
    ELSE.
      CONCATENATE i_parent '-' i_comp-name INTO lv_parent.
    ENDIF.

    TRY.
        DATA(str_type) = CAST cl_abap_structdescr(  cl_abap_structdescr=>describe_by_data_ref( p_data_ref  = i_data ) ).

        APPEND VALUE #( fldname = lv_parent fldtype = field_type-struct ) TO global_field_tab.

        abap_type = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( p_data_ref = i_data ) ).

        check_object( i_abap_type = abap_type i_data = i_data i_parent = lv_parent ).

      CATCH cx_root.
        TRY.
            DATA(table_type) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( p_data_ref = i_data ) ).
            FIELD-SYMBOLS: <table> TYPE ANY TABLE.

            ASSIGN i_data->* TO <table>.
            LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
              EXIT.
            ENDLOOP.

            APPEND VALUE #( fldname = lv_parent fldtype = field_type-table ) TO global_field_tab.

            IF <line> IS ASSIGNED.
              IF <line> IS NOT INITIAL.
                abap_type = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( p_data_ref = <line> ) ).
              ELSE.
                abap_type = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( p_name = 'STRING' ) ).
              ENDIF.
              check_object( i_abap_type = abap_type i_data = <line> i_parent = lv_parent ).
            ENDIF.

          CATCH cx_root.

            IF <line> IS ASSIGNED.  "https://github.com/Jack-Liang/DYNAMIC_DATA/issues/1
              RETURN.
            ENDIF.

            "检查 i_data 的基本类型 Check the basic type of i_data
            IF no_json_type = abap_true.
              APPEND VALUE #( fldname = lv_parent fldtype = field_type-field ) TO global_field_tab.
            ELSE.
              DATA(typedescr) = cl_abap_typedescr=>describe_by_data_ref( i_data ).

              APPEND VALUE #( fldname = lv_parent
                                              fldtype = field_type-field
                                              intty = typedescr->type_kind
                                              lengt = typedescr->length
*                                           DECIM
              ) TO global_field_tab.
            ENDIF.

        ENDTRY.
    ENDTRY.

  ENDMETHOD.


  METHOD check_object.

    LOOP AT i_abap_type->components ASSIGNING FIELD-SYMBOL(<comp>).
      DATA(field) = |i_data->{ <comp>-name }|.
      ASSIGN (field) TO FIELD-SYMBOL(<data>).
      IF <data> IS ASSIGNED AND <data> IS NOT INITIAL.

        check_component(
          i_parent = i_parent
          i_comp   = <comp>
          i_data   = <data> ).

      ENDIF.
      UNASSIGN <data>.
    ENDLOOP.

  ENDMETHOD.


  METHOD deserialize_to_field_tab.
    DATA: i_data    TYPE REF TO data,
          abap_type TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS: <table> TYPE ANY TABLE.

    i_data = /ui2/cl_json=>generate( json = json_data ).

    TRY.
        DATA(abap_type_table) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( p_data_ref = i_data ) ).
        ASSIGN i_data->* TO <table>.
        LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
          EXIT.
        ENDLOOP.
        type = field_type-table.

        abap_type = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( p_data_ref = <line> ) ).

        check_object( i_abap_type = abap_type i_data = <line> i_parent = '' ).


      CATCH cx_sy_move_cast_error.
        type = field_type-struct.

        abap_type = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( p_data_ref = i_data ) ).

        check_object( i_abap_type = abap_type i_data = i_data i_parent = '' ).

    ENDTRY.




  ENDMETHOD.


  METHOD put_parent_field_first.
    CHECK global_field_tab IS NOT INITIAL.

    DATA lt_field LIKE  global_field_tab.
    DATA ls_field LIKE LINE OF global_field_tab.
    DATA itab TYPE TABLE OF string.
    DATA lv_field TYPE string.
    DATA n TYPE i.

    CONSTANTS sep TYPE c VALUE '-'.

    LOOP AT global_field_tab ASSIGNING FIELD-SYMBOL(<fs_field>).
      TRANSLATE <fs_field>-fldname TO UPPER CASE.
    ENDLOOP.

    LOOP AT global_field_tab INTO ls_field.

      IF ls_field-fldname CS sep.
        CLEAR: n, lv_field,itab.

        SPLIT ls_field-fldname AT sep INTO TABLE itab.
        DESCRIBE TABLE itab LINES n.
        DELETE itab INDEX n.

        CONCATENATE LINES OF itab INTO lv_field SEPARATED BY  sep .

        READ TABLE lt_field WITH KEY fldname = lv_field TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          READ TABLE global_field_tab INTO DATA(wa) WITH KEY fldname = lv_field.
          IF sy-subrc = 0.
            APPEND wa TO lt_field.
            DELETE TABLE global_field_tab FROM wa.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ls_field TO lt_field.

    ENDLOOP.

    global_field_tab = lt_field.

  ENDMETHOD.
ENDCLASS.
