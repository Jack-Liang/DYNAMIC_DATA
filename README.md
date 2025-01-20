# ZABAP_TOOLS
ABAP工具包

## 动态创建ABAP数据类型

### 使用方法

```ABAP
  DATA: lr_type TYPE REF TO cl_abap_datadescr.
  DATA: lt_field_tab TYPE TABLE OF zdos_datadescr.
  FIELD-SYMBOLS: <fs_wa> TYPE any.

  INSERT VALUE #( fldname = 'data_Id' fldtype = 'F' )        INTO lt_field_tab INDEX 1.
  INSERT VALUE #( fldname = 'process_Name' fldtype = 'F' )   INTO lt_field_tab INDEX 2.
  INSERT VALUE #( fldname = 'KCFJ' fldtype = 'T' )           INTO lt_field_tab INDEX 3.
  INSERT VALUE #( fldname = 'KCFJ-file_Name' fldtype = 'F' ) INTO lt_field_tab INDEX 4.  "上下级用-连接
  INSERT VALUE #( fldname = 'KCFJ-file_Path' fldtype = 'F' ) INTO lt_field_tab INDEX 5.

  lr_type = zcl_dynamic_object=>create_main( field_tab = lt_field_tab  type = 'S' ).

  CREATE DATA dyn_data TYPE HANDLE lr_type.

  IF dyn_data IS NOT INITIAL.
    ASSIGN dyn_data->*  TO <fs_wa>.
  ENDIF.
```

## 欢迎“一键三连”，欢迎增加新特性
