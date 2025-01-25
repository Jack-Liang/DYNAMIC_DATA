# DYNAMIC_DATA
Create ABAP nested data types dynamically within your program
在程序内动态创建ABAP嵌套数据类型

### USE 使用方法

1. 通过配置字段生成

```ABAP
  DATA: lr_type TYPE REF TO cl_abap_datadescr.
  DATA: lt_field_tab TYPE TABLE OF zdos_datadescr.
  DATA dyn_data TYPE REF TO data.
  FIELD-SYMBOLS: <fs_wa> TYPE any.
  
  INSERT VALUE #( fldname = 'data_Id' fldtype = 'F' )        INTO lt_field_tab INDEX 1.
  INSERT VALUE #( fldname = 'process_Name' fldtype = 'F' )   INTO lt_field_tab INDEX 2.
  INSERT VALUE #( fldname = 'KCFJ' fldtype = 'T' )           INTO lt_field_tab INDEX 3.
  INSERT VALUE #( fldname = 'KCFJ-file_Name' fldtype = 'F' ) INTO lt_field_tab INDEX 4.  "上下级用-连接
  INSERT VALUE #( fldname = 'KCFJ-file_Path' fldtype = 'F' ) INTO lt_field_tab INDEX 5.
  
  lr_type = zcl_dynamic_object=>create_main( field_tab = lt_field_tab  type = 'S' ).
  
  TRY.
      CREATE DATA dyn_data TYPE HANDLE lr_type.
      IF dyn_data IS NOT INITIAL.
        ASSIGN dyn_data->*  TO <fs_wa>.
      ENDIF.
    CATCH cx_root INTO DATA(lr_exc).
      DATA(lv_message) = lr_exc->get_text( ).
      WRITE:/ lv_message.
  ENDTRY.
```

2. 通过json生成
```ABAP
  DATA: lr_type TYPE REF TO cl_abap_datadescr.
  DATA dyn_data TYPE REF TO data.
  DATA json_data TYPE string.

  FIELD-SYMBOLS: <fs_wa> TYPE any.

  json_data = '{  "tenantId": "949043908", "sensorId": "feef4fff-0731-462e-9972-ee92923de3fd", "timestamp": 1600244897484,' &&
               '"measures": [  {  "tagId": "A32F", "zC": 1000.132, "yC": 1000.132, "xC": 324.12, "quality": 92 } ] }'.


  lr_type = zcl_dynamic_object=>create_main( jsondata = jsondata ).
  
  TRY.
      CREATE DATA dyn_data TYPE HANDLE lr_type.
      IF dyn_data IS NOT INITIAL.
        ASSIGN dyn_data->*  TO <fs_wa>.
        /ui2/cl_json=>deserialize( EXPORTING json = json_data  CHANGING data = <fs_wa> ).
      ENDIF.
    CATCH cx_root INTO DATA(lr_exc).
      DATA(lv_message) = lr_exc->get_text( ).
      WRITE:/ lv_message.
  ENDTRY.
```
   

## 欢迎“一键三连”，欢迎增加新特性


相关文章：https://zhuanlan.zhihu.com/p/19400730868

## 鸣谢
[JSON2ABAPType](https://github.com/fidley/JSON2ABAPType)
