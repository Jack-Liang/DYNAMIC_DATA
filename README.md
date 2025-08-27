# DYNAMIC_DATA[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/Greenheart/pagecrypt)
Create ABAP nested data types dynamically within your program

在程序内动态创建 ABAP 嵌套数据类型

## USAGE 使用方法

Use [abapgit](https://github.com/abapGit/docs.abapgit.org) to pull up the project code.

请使用 [abapgit](https://github.com/abapGit/docs.abapgit.org) 拉取项目代码。

### Usage 1 Generate through configuration fields  通过配置字段生成

```ABAP
  DATA: lr_type TYPE REF TO cl_abap_datadescr.
  DATA: lt_field_tab TYPE TABLE OF zdos_datadescr.
  DATA dyn_data TYPE REF TO data.
  FIELD-SYMBOLS: <fs_wa> TYPE any.
  
  INSERT VALUE #( fldname = 'data_Id' fldtype = 'F' )        INTO lt_field_tab INDEX 1.
  INSERT VALUE #( fldname = 'process_Name' fldtype = 'F' )   INTO lt_field_tab INDEX 2.
  INSERT VALUE #( fldname = 'KCFJ' fldtype = 'T' )           INTO lt_field_tab INDEX 3.
  INSERT VALUE #( fldname = 'KCFJ-file_Name' fldtype = 'F' ) INTO lt_field_tab INDEX 4.  "Using ‘-’ to connect superior and subordinate fields
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

### Usage 2 Generate via json  通过 json 生成


```ABAP
  DATA: lr_type TYPE REF TO cl_abap_datadescr.
  DATA dyn_data TYPE REF TO data.
  DATA json_data TYPE string.

  FIELD-SYMBOLS: <fs_wa> TYPE any.

  json_data = '{  "tenantId": "949043908", "sensorId": "feef4fff-0731-462e-9972-ee92923de3fd", "timestamp": 1600244897484,' &&
               '"measures": [  {  "tagId": "A32F", "zC": 1000.132, "yC": 1000.132, "xC": 324.12, "quality": 92 } ] }'.


  lr_type = zcl_dynamic_object=>create_main( JSON_DATA = json_data ).
  
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
   
### Usage 3 Created by basic type  通过基本类型创建

In Usage 1, if you pass a STRUF, such as SFLIGHT-CARRID or S_CARR_ID, to field_tab, the corresponding type is generated instead of the default String type.
You can also specify the underlying ABAP type, such as `( INTTY = 'C' LENGT= 50 )` or `( INTTY = 'P' LENGT = 8 DECIM = 2 )`, which will create a variable of the specified type and length.

在 Usage 1 中，如果给 field_tab 传入 STRUF，如 `SFLIGHT-CARRID` 或 `S_CARR_ID`，则会生成对应类型，而不是默认的 String 类型；
也可以指定基础的 ABAP 类型，例如 `( INTTY = 'C' LENGT = 50 )` 或者 `( INTTY = 'P' LENGT = 8 DECIM = 2 )`, 这将会创建指定类型和长度的变量。

## ⚠️ Notion 重要说明

This is a nascent project and the authors can not guarantee that it will always work correctly, so please do NOT use it in a production environment.

If you pass `NO_TYPE = ''` to CREATE_MAIN when generating via json, the program will infer the possible data types based on json. **The results obtained in this way are not always accurate**.

这是一个新生的项目，作者不能保证它始终正确运行，因此请不要用于生产环境。

如果在通过 json 生成时，如果给 CREATE_MAIN 传入 `NO_TYPE = ''`, 程序将根据 json 推测可能的数据类型，需要说明的，**这种方式得到的结果并不一定是准确的**。

## 🌟 Looking forward to your suggestions 欢迎“一键三连”，欢迎增加新特性

Related Articles 相关文章：https://zhuanlan.zhihu.com/p/19400730868

## This repository is included in [dotabap](https://dotabap.org/)

## Thanks 鸣谢

This project also refers to some code from the following project, and we hereby express our gratitude

[JSON2ABAPType](https://github.com/fidley/JSON2ABAPType)
