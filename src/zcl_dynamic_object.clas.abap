class ZCL_DYNAMIC_OBJECT definition
  public
  final
  create public .

public section.

  types:
    ty_split TYPE TABLE OF string .

  class-methods CREATE_MAIN
    importing
      value(FIELD_TAB) type ZDOT_DATADESCR
      value(TYPE) type ZDOE_FLDTYPE
    returning
      value(REF_TYPE) type ref to CL_ABAP_DATADESCR
    exceptions
      UNSUPPORTED_TYPE
      EXECUTION_FAILED .
  PROTECTED SECTION.




private section.

  class-data GLOBAL_FIELD_TAB type ZDOT_DATADESCR .
  constants:
    BEGIN OF field_type,
                 field  TYPE char1  VALUE 'F',
                 struct TYPE char1  VALUE 'S',
                 table  TYPE char1  VALUE 'T',
               END OF field_type .

  class-methods CREATE_BY_FIELD_TAB
    importing
      value(TYPE) type ZDOE_FLDTYPE
    exporting
      value(REF_TYPE) type ref to CL_ABAP_DATADESCR
      value(REF_DATA) type ref to DATA
    changing
      !FIELD_TAB type ZDOT_DATADESCR .
  class-methods APPEND_FIELD
    importing
      !FLDNAME type DATA
      !METHOD type ZDOE_METHOD
      !OBJECT type DATA
    changing
      !COMP_TAB type ABAP_COMPONENT_TAB .
  class-methods STRUCTURAL_SUB
    changing
      !FIELD_TAB type ZDOT_DATADESCR
      !SPLIT_TAB type TY_SPLIT .
ENDCLASS.



CLASS ZCL_DYNAMIC_OBJECT IMPLEMENTATION.


  METHOD append_field.


    DATA ls_comp TYPE LINE OF abap_component_tab.

    ls_comp-name = fldname.
    CASE method.
      WHEN 'A' .
        ls_comp-type ?= cl_abap_tabledescr=>describe_by_data( object ).
      WHEN 'B'.
        ls_comp-type ?= cl_abap_tabledescr=>describe_by_name( object ).
      WHEN 'C' .
        ls_comp-type ?= cl_abap_tabledescr=>describe_by_object_ref( object ).
      WHEN 'D' .
        ls_comp-type ?= cl_abap_tabledescr=>describe_by_data_ref( object ).
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
        WHEN 'F'.
          append_field( EXPORTING fldname = <fs_split>
                                  method =  'B'
                                  object = 'STRING'
                                  CHANGING comp_tab = lt_comp[] ).
        WHEN 'S'.

          "构造下级字段列表
          structural_sub( CHANGING field_tab = lt_datadescr split_tab = lt_split  ).

          create_by_field_tab( EXPORTING type = <fs_datadescr>-fldtype
                               IMPORTING ref_data = l_dyn_obj
                               CHANGING  field_tab = lt_datadescr ).

          append_field( EXPORTING fldname = <fs_split>
                                  method =  'D'
                                  object = l_dyn_obj"创建好的类型
                                  CHANGING comp_tab = lt_comp[] ).
        WHEN 'T'.

          "构造下级字段列表
          structural_sub( CHANGING field_tab = lt_datadescr split_tab = lt_split  ).

          create_by_field_tab( EXPORTING type = <fs_datadescr>-fldtype
                               IMPORTING ref_data = l_dyn_obj
                               CHANGING  field_tab = lt_datadescr ).

          append_field( EXPORTING fldname = <fs_split>
                                  method =  'D'
                                  object = l_dyn_obj"创建好的类型
                                  CHANGING comp_tab = lt_comp[] ).

        WHEN OTHERS.

      ENDCASE.

    ENDLOOP.


    IF lt_comp IS NOT INITIAL.

      lr_struc = cl_abap_structdescr=>create( lt_comp ).

      IF type = 'S'.
        CREATE DATA ref_data  TYPE HANDLE lr_struc.
        ref_type ?= lr_struc.
      ELSEIF type = 'T'.
        lr_table = cl_abap_tabledescr=>create( lr_struc ).
        CREATE DATA ref_data  TYPE HANDLE lr_table.
        ref_type ?= lr_table.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD create_main.


    "check
    IF type <> field_type-struct AND type <> field_type-table.
      RAISE unsupported_type.
    ENDIF.

    IF field_tab[] IS INITIAL.
      RAISE execution_failed.
    ENDIF.





    global_field_tab[] = field_tab[].
    create_by_field_tab( EXPORTING type = type
                         IMPORTING ref_type = ref_type
                         CHANGING field_tab = global_field_tab ).


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
ENDCLASS.
