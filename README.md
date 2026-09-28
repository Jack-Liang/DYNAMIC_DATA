# DYNAMIC_DATA [![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/Jack-Liang/DYNAMIC_DATA) [![abaplint](https://github.com/Jack-Liang/DYNAMIC_DATA/actions/workflows/abaplint.yml/badge.svg)](https://github.com/Jack-Liang/DYNAMIC_DATA/actions/workflows/abaplint.yml)
Create ABAP nested data types dynamically within your program

在程序内动态创建 ABAP 嵌套数据类型

## USAGE 使用方法

Use [abapgit](https://github.com/abapGit/docs.abapgit.org) to pull up the project code.

请使用 [abapgit](https://github.com/abapGit/docs.abapgit.org) 拉取项目代码。

A runnable demo / smoke test report `ZDYNAMIC_DATA_DEMO` covering the examples below and the edge cases is included in the package. It is not required by the library and can be deleted after pulling.

包内附带一个可运行的演示/冒烟测试程序 `ZDYNAMIC_DATA_DEMO`，覆盖下文示例及边界场景；该程序不是类库运行所必需的，拉取后可以删除。

### Usage 1 Generate through configuration fields  通过配置字段生成

```ABAP
  DATA: lr_type TYPE REF TO cl_abap_datadescr.
  DATA: lt_field_tab TYPE TABLE OF zdos_datadescr.
  DATA dyn_data TYPE REF TO data.
  FIELD-SYMBOLS: <fs_wa> TYPE any.
  
  INSERT VALUE #( fldname = 'hello'        fldtype = 'F' )        INTO lt_field_tab INDEX 1.
  INSERT VALUE #( fldname = 'author'       fldtype = 'F' )        INTO lt_field_tab INDEX 2.
  INSERT VALUE #( fldname = 'skills'       fldtype = 'T' )        INTO lt_field_tab INDEX 3.
  INSERT VALUE #( fldname = 'skills-name'  fldtype = 'F' )        INTO lt_field_tab INDEX 4.  "Using ‘-’ to connect superior and subordinate fields
  INSERT VALUE #( fldname = 'skills-level' fldtype = 'F' ) INTO lt_field_tab INDEX 5.
  
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

The JSON is parsed with the kernel `sXML` library, so type generation itself has **no dependencies on other repositories** (in the example below `/ui2/cl_json` is only used afterwards to fill the generated type with data).

JSON 解析基于内核的 `sXML` 库实现，类型生成本身**不依赖任何其他仓库**（下例中的 `/ui2/cl_json` 只是随后往生成的类型里填充数据时由调用方自己使用的）。


```ABAP
  DATA: lr_type TYPE REF TO cl_abap_datadescr.
  DATA dyn_data TYPE REF TO data.
  DATA json_data TYPE string.

  FIELD-SYMBOLS: <fs_wa> TYPE any.

  json_data = '{ "hello": "Hi! I am a nested data type created dynamically at runtime.",' &&
               '"author": { "name": "Jack Liang", "favLang": "ABAP", "github": "Jack-Liang/DYNAMIC_DATA" },' &&
               '"skills": [ { "name": "ABAP", "level": 95 }, { "name": "JSON", "level": 88 }, { "name": "Coffee", "level": 100 } ] }'.


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
   
Since v2.1.0 you can also get a **filled data object in one step** — the JSON is parsed once and no JSON binder is needed on the caller side (booleans become `X`/initial, `null` stays initial). 自 v2.1.0 起也可以**一步拿到填好值的数据对象**——JSON 只解析一次，调用方不再需要任何 JSON 反序列化组件（布尔值填 `X`/初始，`null` 保持初始）：

```ABAP
  DATA dyn_data TYPE REF TO data.
  FIELD-SYMBOLS: <fs_wa> TYPE any.

  dyn_data = zcl_dynamic_object=>create_data( json_data = json_data no_type = '' ).

  ASSIGN dyn_data->* TO <fs_wa>.  "<fs_wa> already contains the values 已填好值
```

> Note: to handle the classic exceptions (`INVALID_JSON` etc.) use the `CALL METHOD ... EXCEPTIONS` form, same as with `CREATE_MAIN`. 注：如需处理经典异常（`INVALID_JSON` 等），与 `CREATE_MAIN` 一样使用 `CALL METHOD ... EXCEPTIONS` 调用形式。

### Usage 3 Created by basic type  通过基本类型创建

In Usage 1, if you pass a STRUF, such as SFLIGHT-CARRID or S_CARR_ID, to field_tab, the corresponding type is generated instead of the default String type.
You can also specify the underlying ABAP type, such as `( INTTY = 'C' LENGT= 50 )` or `( INTTY = 'P' LENGT = 8 DECIM = 2 )`, which will create a variable of the specified type and length.

在 Usage 1 中，如果给 field_tab 传入 STRUF，如 `SFLIGHT-CARRID` 或 `S_CARR_ID`，则会生成对应类型，而不是默认的 String 类型；
也可以指定基础的 ABAP 类型，例如 `( INTTY = 'C' LENGT = 50 )` 或者 `( INTTY = 'P' LENGT = 8 DECIM = 2 )`, 这将会创建指定类型和长度的变量。

## ⚠️ Notion 重要说明

This is a new project. The author cannot guarantee that it will always run correctly. Therefore, please test it thoroughly before using it in a production environment.

If you pass `NO_TYPE = ''` to CREATE_MAIN when generating via json, the program will infer the possible data types based on json:

| JSON | Inferred ABAP type |
| --- | --- |
| `95` | `i` (small integers, up to 9 digits become `p` otherwise) |
| `88.5` | `p` with length/decimals fitted to the literal |
| `true` / `false` | `c` length 1 |
| `"text"` / `null` / `1e5` | `string` |
| `[]` , `["a", "b"]` | table of `string` |
| `[10, 20]` | table of `i` (consistent scalar items) |

Errors are reported through the classic exceptions `INVALID_JSON` (unparsable JSON) and `INVALID_FIELD_NAME` (a key longer than 30 characters, containing characters other than `A-Z 0-9 _`, starting with a digit, or containing `-`, which is reserved as the hierarchy separator).

这是一个新项目。作者不能保证它总是正确运行。因此，在将其用于生产环境之前，请对其进行彻底的测试。

如果在通过 json 生成时，给 CREATE_MAIN 传入 `NO_TYPE = ''`，程序将根据 json 推断可能的数据类型，规则见上表；推断不出类型的值一律按 `string` 处理。解析失败会抛出经典异常 `INVALID_JSON`（无效 JSON）和 `INVALID_FIELD_NAME`（字段名超过 30 位、含 `A-Z 0-9 _` 之外的字符、以数字开头，或包含层级分隔符 `-`）。

## 🌟 Looking forward to your suggestions 欢迎“一键三连”，欢迎增加新特性

Related Articles 相关文章：

1. [在程序内动态创建ABAP嵌套数据类型](https://zhuanlan.zhihu.com/p/19400730868)
2. [abapGit 使用经验总结](https://zhuanlan.zhihu.com/p/20034587426)

## This repository is included in [dotabap](https://dotabap.org/)

## Thanks 鸣谢

This project also refers to some code from the following projects, and we hereby express our gratitude

[JSON2ABAPType](https://github.com/fidley/JSON2ABAPType)

[ajson](https://github.com/sbcgua/ajson) (the sXML based JSON parsing approach, MIT)
