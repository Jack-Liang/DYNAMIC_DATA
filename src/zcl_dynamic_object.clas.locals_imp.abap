"* Local JSON parsing for ZCL_DYNAMIC_OBJECT.
"*
"* The parsing approach (kernel sXML reader + flat node table) is borrowed
"* from sbcgua/ajson (MIT license): https://github.com/sbcgua/ajson
"*
"* The sXML JSON reader maps every JSON value to an element whose name is
"* the value type ('object', 'array', 'str', 'num', 'bool', 'null') and
"* puts the object member name into a 'name' attribute. Number literals
"* are delivered as their source text, which is what makes precise type
"* inference (length, decimals) possible.

CLASS lcl_json_parser DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_node,
        path     TYPE string,
        name     TYPE string,
        kind     TYPE string,
        value    TYPE string,
        children TYPE i,
      END OF ty_node,
      ty_nodes TYPE STANDARD TABLE OF ty_node WITH NON-UNIQUE KEY path name.

    CONSTANTS:
      BEGIN OF c_kind,
        object TYPE string VALUE `object`,
        array  TYPE string VALUE `array`,
        string TYPE string VALUE `str`,
        number TYPE string VALUE `num`,
        bool   TYPE string VALUE `bool`,
        null   TYPE string VALUE `null`,
      END OF c_kind.

    CLASS-METHODS parse
      IMPORTING
        !json        TYPE string
      RETURNING
        VALUE(nodes) TYPE ty_nodes
      RAISING
        zcx_dynamic_json_error.

ENDCLASS.

CLASS lcl_json_parser IMPLEMENTATION.

  METHOD parse.

    DATA lt_stack TYPE STANDARD TABLE OF REF TO ty_node.
    DATA lv_stack_path TYPE string.
    DATA lr_top TYPE REF TO ty_node.
    DATA lo_node TYPE REF TO if_sxml_node.
    DATA lo_open TYPE REF TO if_sxml_open_element.
    DATA lo_close TYPE REF TO if_sxml_close_element.
    DATA lo_value TYPE REF TO if_sxml_value_node.
    DATA lt_attributes TYPE if_sxml_attribute=>attributes.
    DATA lo_attribute LIKE LINE OF lt_attributes.
    DATA lo_reader TYPE REF TO if_sxml_reader.

    FIELD-SYMBOLS <node> TYPE ty_node.

    DATA(lv_json_utf8) = cl_abap_codepage=>convert_to( source = json codepage = `UTF-8` ).
    lo_reader = cl_sxml_string_reader=>create( lv_json_utf8 ).

    TRY.
        DO.
          lo_node = lo_reader->read_next_node( ).
          IF lo_node IS NOT BOUND.
            EXIT.
          ENDIF.

          CASE lo_node->type.
            WHEN if_sxml_node=>co_nt_element_open.

              lo_open ?= lo_node.
              APPEND INITIAL LINE TO nodes ASSIGNING <node>.
              <node>-kind = lo_open->qname-name.

              READ TABLE lt_stack INDEX 1 INTO lr_top.
              IF sy-subrc = 0.
                " Child node: path is the accumulated path of the parent
                <node>-path = lv_stack_path.
                lr_top->children = lr_top->children + 1.
                IF lr_top->kind = c_kind-array.
                  " Array items have no name attribute, they get their index
                  <node>-name = |{ lr_top->children }|.
                ELSE.
                  " JSON object members always have one 'name' attribute
                  lt_attributes = lo_open->get_attributes( ).
                  READ TABLE lt_attributes INTO lo_attribute INDEX 1.
                  IF sy-subrc <> 0 OR lo_attribute->qname-name <> `name`.
                    RAISE EXCEPTION TYPE zcx_dynamic_json_error
                      EXPORTING i_text = `Node without name attribute (not JSON?)`.
                  ENDIF.
                  <node>-name = lo_attribute->get_value( ).
                ENDIF.
              ENDIF.

              GET REFERENCE OF <node> INTO lr_top.
              INSERT lr_top INTO lt_stack INDEX 1.
              lv_stack_path = lv_stack_path && <node>-name && '/'.

            WHEN if_sxml_node=>co_nt_element_close.

              lo_close ?= lo_node.
              READ TABLE lt_stack INDEX 1 INTO lr_top.
              IF sy-subrc <> 0.
                RAISE EXCEPTION TYPE zcx_dynamic_json_error
                  EXPORTING i_text = `Unexpected closing node`.
              ENDIF.
              IF lo_close->qname-name <> lr_top->kind.
                RAISE EXCEPTION TYPE zcx_dynamic_json_error
                  EXPORTING i_text = `Unexpected closing node type`.
              ENDIF.
              DELETE lt_stack INDEX 1.
              " Remove the last path component
              lv_stack_path = substring( val = lv_stack_path
                                         len = find( val = lv_stack_path sub = '/' occ = -2 ) + 1 ).

            WHEN if_sxml_node=>co_nt_value.

              lo_value ?= lo_node.
              IF <node> IS ASSIGNED.
                " Value of the scalar element that is currently open
                <node>-value = lo_value->get_value( ).
              ENDIF.

            WHEN OTHERS.
              RAISE EXCEPTION TYPE zcx_dynamic_json_error
                EXPORTING i_text = `Unexpected node type`.
          ENDCASE.
        ENDDO.

        IF lines( lt_stack ) > 0.
          RAISE EXCEPTION TYPE zcx_dynamic_json_error
            EXPORTING i_text = `Unexpected end of data`.
        ENDIF.
        IF lines( nodes ) = 0.
          RAISE EXCEPTION TYPE zcx_dynamic_json_error
            EXPORTING i_text = `No JSON data found`.
        ENDIF.

    CATCH cx_sxml_parse_error INTO DATA(lx_parse).
      RAISE EXCEPTION TYPE zcx_dynamic_json_error
        EXPORTING i_text = lx_parse->get_text( ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


"* Walks the parsed JSON node table and converts it into the flat
"* field description table (ZDOT_DATADESCR) that CREATE_MAIN builds on.
"*
"* Field name convention: hierarchy is joined with '-', children of an
"* array are attached to the array itself (no item index in the name),
"* so ['skills-name', 'skills-level'] describes the line structure of
"* the 'skills' table. Duplicate field names keep their first occurrence.

CLASS lcl_json_walker DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_elem_descr,
        intty TYPE inttype,
        lengt TYPE ilen,
        decim TYPE decimals,
      END OF ty_elem_descr.

    CONSTANTS:
      c_fieldtype_field  TYPE zdoe_fldtype VALUE 'F',
      c_fieldtype_struct TYPE zdoe_fldtype VALUE 'S',
      c_fieldtype_table  TYPE zdoe_fldtype VALUE 'T'.

    CLASS-METHODS walk
      IMPORTING
        !json        TYPE string
        !infer_types TYPE abap_bool
      EXPORTING
        !root_type     TYPE zdoe_fldtype
        !root_is_array TYPE abap_bool
        !rows          TYPE zdot_datadescr
      RAISING
        zcx_dynamic_json_error
        zcx_dynamic_name_error.

    CLASS-METHODS walk_nodes
      IMPORTING
        !nodes       TYPE lcl_json_parser=>ty_nodes
        !infer_types TYPE abap_bool
      EXPORTING
        !root_type     TYPE zdoe_fldtype
        !root_is_array TYPE abap_bool
        !rows          TYPE zdot_datadescr
      RAISING
        zcx_dynamic_json_error
        zcx_dynamic_name_error.

  PRIVATE SECTION.

    CLASS-METHODS walk_object
      IMPORTING
        !nodes       TYPE lcl_json_parser=>ty_nodes
        !prefix      TYPE string
        !node_path   TYPE string
        !infer_types TYPE abap_bool
      CHANGING
        !rows        TYPE zdot_datadescr
      RAISING
        zcx_dynamic_name_error.

    CLASS-METHODS walk_array
      IMPORTING
        !nodes       TYPE lcl_json_parser=>ty_nodes
        !prefix      TYPE string
        !node_path   TYPE string
        !infer_types TYPE abap_bool
      CHANGING
        !rows        TYPE zdot_datadescr
      RAISING
        zcx_dynamic_name_error.

    CLASS-METHODS emit_row
      IMPORTING
        !full_name TYPE string
        !fldtype   TYPE zdoe_fldtype
        !descr     TYPE ty_elem_descr OPTIONAL
      CHANGING
        !rows      TYPE zdot_datadescr.

    CLASS-METHODS validate_name
      IMPORTING
        !name TYPE string
      RAISING
        zcx_dynamic_name_error.

    CLASS-METHODS join_prefix
      IMPORTING
        !prefix        TYPE string
        !name          TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS number_parts
      IMPORTING
        !literal    TYPE string
      EXPORTING
        !int_digits TYPE i
        !dec_digits TYPE i
        !inferable  TYPE abap_bool.

    CLASS-METHODS build_descr
      IMPORTING
        !int_digits TYPE i
        !dec_digits TYPE i
      RETURNING
        VALUE(descr) TYPE ty_elem_descr.

    CLASS-METHODS infer_number_descr
      IMPORTING
        !literal       TYPE string
      RETURNING
        VALUE(descr) TYPE ty_elem_descr.

ENDCLASS.

CLASS lcl_json_walker IMPLEMENTATION.

  METHOD walk.

    walk_nodes( EXPORTING nodes       = lcl_json_parser=>parse( json )
                         infer_types = infer_types
               IMPORTING root_type     = root_type
                         root_is_array = root_is_array
                         rows          = rows ).

  ENDMETHOD.


  METHOD walk_nodes.

    " Index 1 is the root node (empty path and name)
    READ TABLE nodes ASSIGNING FIELD-SYMBOL(<root>) INDEX 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dynamic_json_error
        EXPORTING i_text = `No JSON data found`.
    ENDIF.

    CASE <root>-kind.
      WHEN lcl_json_parser=>c_kind-object.
        root_type = c_fieldtype_struct.
        walk_object( EXPORTING nodes = nodes
                               prefix = ''
                               node_path = '/'
                               infer_types = infer_types
                     CHANGING  rows = rows ).
      WHEN lcl_json_parser=>c_kind-array.
        root_type = c_fieldtype_table.
        root_is_array = abap_true.
        walk_array( EXPORTING nodes = nodes
                              prefix = ''
                              node_path = '/'
                              infer_types = infer_types
                    CHANGING  rows = rows ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_dynamic_json_error
          EXPORTING i_text = `Root of the JSON must be an object or an array`.
    ENDCASE.

  ENDMETHOD.


  METHOD walk_object.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<child>) WHERE path = node_path.

      validate_name( to_upper( <child>-name ) ).
      DATA(lv_child_prefix) = join_prefix( prefix = prefix name = to_upper( <child>-name ) ).

      CASE <child>-kind.
        WHEN lcl_json_parser=>c_kind-object.
          emit_row( EXPORTING full_name = lv_child_prefix
                              fldtype   = c_fieldtype_struct
                    CHANGING  rows      = rows ).
          walk_object( EXPORTING nodes       = nodes
                                prefix      = lv_child_prefix
                                node_path   = <child>-path && <child>-name && '/'
                                infer_types = infer_types
                      CHANGING  rows        = rows ).

        WHEN lcl_json_parser=>c_kind-array.
          walk_array( EXPORTING nodes       = nodes
                               prefix      = lv_child_prefix
                               node_path   = <child>-path && <child>-name && '/'
                               infer_types = infer_types
                     CHANGING  rows        = rows ).

        WHEN lcl_json_parser=>c_kind-number.
          emit_row( EXPORTING full_name = lv_child_prefix
                              fldtype   = c_fieldtype_field
                              descr     = COND #( WHEN infer_types = abap_true
                                                  THEN infer_number_descr( <child>-value )
                                                  ELSE VALUE #( ) )
                    CHANGING  rows      = rows ).

        WHEN lcl_json_parser=>c_kind-bool.
          emit_row( EXPORTING full_name = lv_child_prefix
                              fldtype   = c_fieldtype_field
                              descr     = COND #( WHEN infer_types = abap_true
                                                  THEN VALUE #( intty = 'C' lengt = 1 )
                                                  ELSE VALUE #( ) )
                    CHANGING  rows      = rows ).

        WHEN OTHERS.
          " str / null -> default STRING
          emit_row( EXPORTING full_name = lv_child_prefix
                              fldtype   = c_fieldtype_field
                    CHANGING  rows      = rows ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD walk_array.

    DATA:
      lv_any_object     TYPE abap_bool VALUE abap_false,
      lv_any_array      TYPE abap_bool VALUE abap_false,
      lv_any_number     TYPE abap_bool VALUE abap_false,
      lv_any_bool       TYPE abap_bool VALUE abap_false,
      lv_any_text       TYPE abap_bool VALUE abap_false,
      lv_number_ok      TYPE abap_bool VALUE abap_true,
      lv_max_int_digits TYPE i,
      lv_max_dec_digits TYPE i.

    IF prefix IS NOT INITIAL.
      emit_row( EXPORTING full_name = prefix
                        fldtype   = c_fieldtype_table
              CHANGING  rows      = rows ).
    ENDIF.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<item>) WHERE path = node_path.

      CASE <item>-kind.
        WHEN lcl_json_parser=>c_kind-object.
          " Fields of every item become fields of the table line structure
          lv_any_object = abap_true.
          walk_object( EXPORTING nodes       = nodes
                                prefix      = prefix
                                node_path   = <item>-path && <item>-name && '/'
                                infer_types = infer_types
                      CHANGING  rows        = rows ).

        WHEN lcl_json_parser=>c_kind-array.
          " Nested arrays are not supported, fall back to string
          lv_any_array = abap_true.

        WHEN lcl_json_parser=>c_kind-number.
          lv_any_number = abap_true.
          number_parts( EXPORTING literal    = <item>-value
                        IMPORTING int_digits = DATA(lv_int_digits)
                                  dec_digits = DATA(lv_dec_digits)
                                  inferable  = DATA(lv_inferable) ).
          IF lv_inferable = abap_false.
            lv_number_ok = abap_false.
          ENDIF.
          IF lv_int_digits > lv_max_int_digits.
            lv_max_int_digits = lv_int_digits.
          ENDIF.
          IF lv_dec_digits > lv_max_dec_digits.
            lv_max_dec_digits = lv_dec_digits.
          ENDIF.

        WHEN lcl_json_parser=>c_kind-bool.
          lv_any_bool = abap_true.

        WHEN OTHERS.
          " str / null
          lv_any_text = abap_true.

      ENDCASE.

    ENDLOOP.

    IF prefix IS INITIAL OR infer_types = abap_false
      OR lv_any_object = abap_true OR lv_any_array = abap_true.
      RETURN.
    ENDIF.

    " Consistent scalar items: type the table line accordingly
    IF lv_any_number = abap_true AND lv_number_ok = abap_true
      AND lv_any_bool = abap_false AND lv_any_text = abap_false.
      DATA(lv_descr) = build_descr( int_digits = lv_max_int_digits
                                    dec_digits = lv_max_dec_digits ).
      IF lv_descr-intty IS NOT INITIAL.
        READ TABLE rows ASSIGNING FIELD-SYMBOL(<row>) WITH KEY fldname = prefix.
        IF sy-subrc = 0.
          <row>-intty = lv_descr-intty.
          <row>-lengt = lv_descr-lengt.
          <row>-decim = lv_descr-decim.
        ENDIF.
      ENDIF.
    ELSEIF lv_any_bool = abap_true
      AND lv_any_number = abap_false AND lv_any_text = abap_false.
      READ TABLE rows ASSIGNING <row> WITH KEY fldname = prefix.
      IF sy-subrc = 0.
        <row>-intty = 'C'.
        <row>-lengt = 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD emit_row.

    " First occurrence wins, later duplicates are ignored
    READ TABLE rows TRANSPORTING NO FIELDS WITH KEY fldname = full_name.
    IF sy-subrc <> 0.
      rows = VALUE #( BASE rows
                      ( fldname = full_name
                        fldtype = fldtype
                        intty   = descr-intty
                        lengt   = descr-lengt
                        decim   = descr-decim ) ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_name.

    " ABAP component name rules: max 30 characters, A-Z/0-9/_ only,
    " must not start with a digit. '-' cannot be allowed because it is
    " the hierarchy separator of the field description table.
    FIND REGEX '^[A-Z_][A-Z_0-9]*$' IN name ##REGEX_POSIX.
    IF sy-subrc <> 0 OR strlen( name ) > 30.
      RAISE EXCEPTION TYPE zcx_dynamic_name_error
        EXPORTING i_text = |Invalid JSON key for an ABAP component name: "{ name }"|.
    ENDIF.

  ENDMETHOD.


  METHOD join_prefix.

    IF prefix IS INITIAL.
      result = name.
    ELSE.
      result = |{ prefix }-{ name }|.
    ENDIF.

  ENDMETHOD.


  METHOD number_parts.

    DATA lv_int_part TYPE string.
    DATA lv_dec_part TYPE string.

    CLEAR: int_digits, dec_digits.
    inferable = abap_false.

    " Scientific notation is not inferred -> stays string
    IF literal CS 'e' OR literal CS 'E'.
      RETURN.
    ENDIF.

    SPLIT literal AT '.' INTO lv_int_part lv_dec_part.
    IF lv_int_part CP '+*' OR lv_int_part CP '-*'.
      SHIFT lv_int_part.
    ENDIF.

    IF lv_int_part CO '0123456789'
      AND ( lv_dec_part IS INITIAL OR lv_dec_part CO '0123456789' ).
      int_digits = strlen( lv_int_part ).
      dec_digits = strlen( lv_dec_part ).
      inferable = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD build_descr.

    " Small integers become INT4, everything else a packed number that
    " fits the digits of the literal (2*L-1 digits fit into P of length L)
    IF dec_digits = 0 AND int_digits <= 9.
      descr-intty = 'I'.
    ELSE.
      DATA(lv_total) = int_digits + dec_digits.
      DATA(lv_length) = ( lv_total + 3 ) DIV 2.
      IF dec_digits > 14 OR lv_length > 16.
        RETURN. " stays string
      ENDIF.
      descr-intty = 'P'.
      descr-lengt = lv_length.
      descr-decim = dec_digits.
    ENDIF.

  ENDMETHOD.


  METHOD infer_number_descr.

    number_parts( EXPORTING literal    = literal
                  IMPORTING int_digits = DATA(lv_int_digits)
                            dec_digits = DATA(lv_dec_digits)
                            inferable  = DATA(lv_inferable) ).
    IF lv_inferable = abap_true.
      descr = build_descr( int_digits = lv_int_digits
                           dec_digits = lv_dec_digits ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.


"* Fills a generated dynamic data object with values from the parsed
"* JSON node table. The data object must have been generated from the
"* same node table (ZCL_DYNAMIC_OBJECT=>CREATE_DATA), so every JSON
"* member finds its component. Booleans become 'X'/initial, null and
"* unsupported nested arrays stay initial.

CLASS lcl_json_filler DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS fill
      IMPORTING
        !nodes TYPE lcl_json_parser=>ty_nodes
        !data  TYPE REF TO data
      RAISING
        zcx_dynamic_json_error.

  PRIVATE SECTION.
    CLASS-METHODS:
      fill_structure
        IMPORTING
          !nodes     TYPE lcl_json_parser=>ty_nodes
          !node_path TYPE string
        CHANGING
          !c_data    TYPE any
        RAISING
          zcx_dynamic_json_error,
      fill_table
        IMPORTING
          !nodes     TYPE lcl_json_parser=>ty_nodes
          !node_path TYPE string
        CHANGING
          !c_data    TYPE STANDARD TABLE
        RAISING
          zcx_dynamic_json_error,
      set_value
        IMPORTING
          !node   TYPE lcl_json_parser=>ty_node
        CHANGING
          !c_data TYPE any.

ENDCLASS.

CLASS lcl_json_filler IMPLEMENTATION.

  METHOD fill.

    READ TABLE nodes ASSIGNING FIELD-SYMBOL(<root>) INDEX 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_dynamic_json_error
        EXPORTING i_text = `No JSON data found`.
    ENDIF.

    ASSIGN data->* TO FIELD-SYMBOL(<data>).

    CASE <root>-kind.
      WHEN lcl_json_parser=>c_kind-object.
        fill_structure( EXPORTING nodes = nodes
                                node_path = '/'
                        CHANGING  c_data = <data> ).
      WHEN lcl_json_parser=>c_kind-array.
        fill_table( EXPORTING nodes = nodes
                              node_path = '/'
                    CHANGING  c_data = <data> ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_dynamic_json_error
          EXPORTING i_text = `Root of the JSON must be an object or an array`.
    ENDCASE.

  ENDMETHOD.


  METHOD fill_structure.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<child>) WHERE path = node_path.

      ASSIGN COMPONENT to_upper( <child>-name ) OF STRUCTURE c_data
        TO FIELD-SYMBOL(<comp>).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_dynamic_json_error
          EXPORTING i_text = |Component for JSON key "{ <child>-name }" not found|.
      ENDIF.

      CASE <child>-kind.
        WHEN lcl_json_parser=>c_kind-object.
          fill_structure( EXPORTING nodes     = nodes
                                  node_path = <child>-path && <child>-name && '/'
                          CHANGING  c_data    = <comp> ).
        WHEN lcl_json_parser=>c_kind-array.
          fill_table( EXPORTING nodes     = nodes
                               node_path = <child>-path && <child>-name && '/'
                      CHANGING  c_data    = <comp> ).
        WHEN OTHERS.
          set_value( EXPORTING node = <child> CHANGING c_data = <comp> ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_table.

    LOOP AT nodes ASSIGNING FIELD-SYMBOL(<item>) WHERE path = node_path.

      APPEND INITIAL LINE TO c_data ASSIGNING FIELD-SYMBOL(<line>).

      CASE <item>-kind.
        WHEN lcl_json_parser=>c_kind-object.
          fill_structure( EXPORTING nodes     = nodes
                                  node_path = <item>-path && <item>-name && '/'
                          CHANGING  c_data    = <line> ).
        WHEN lcl_json_parser=>c_kind-number OR lcl_json_parser=>c_kind-string
          OR lcl_json_parser=>c_kind-bool.
          set_value( EXPORTING node = <item> CHANGING c_data = <line> ).
        WHEN OTHERS.
          " null / nested arrays stay initial
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_value.

    CASE node-kind.
      WHEN lcl_json_parser=>c_kind-number OR lcl_json_parser=>c_kind-string.
        " Numeric literals are kept as source text, the assignment
        " converts them into the generated I/P/STRING component
        c_data = node-value.
      WHEN lcl_json_parser=>c_kind-bool.
        IF node-value = 'true'.
          c_data = abap_true.
        ELSE.
          CLEAR c_data.
        ENDIF.
      WHEN OTHERS.
        " null stays initial
    ENDCASE.

  ENDMETHOD.

ENDCLASS.


"* Tree based type builder.
"*
"* The flat field description rows ('A-B-C' paths) are parsed into a
"* tree of LCL_TREE_NODE objects first, the types are then generated
"* bottom up from the tree. This makes the row order irrelevant and
"* replaces the former call stack depth detection, flag consumption
"* and parent reordering logic.
"*
"* An intermediate path segment without an own row defaults to a
"* structure instead of being dropped or leaking into the parent.

CLASS lcl_tree_node DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES ty_children TYPE STANDARD TABLE OF REF TO lcl_tree_node WITH DEFAULT KEY.

    DATA ms_row TYPE zdos_datadescr.
    DATA mt_children TYPE ty_children.

ENDCLASS.

CLASS lcl_tree_node IMPLEMENTATION.
ENDCLASS.


CLASS lcl_type_builder DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CONSTANTS:
      c_fieldtype_field  TYPE zdoe_fldtype VALUE 'F',
      c_fieldtype_struct TYPE zdoe_fldtype VALUE 'S',
      c_fieldtype_table  TYPE zdoe_fldtype VALUE 'T'.

    TYPES ty_nodes TYPE STANDARD TABLE OF REF TO lcl_tree_node WITH DEFAULT KEY.

    CLASS-METHODS build
      IMPORTING
        !rows       TYPE zdot_datadescr
        !type       TYPE zdoe_fldtype
      RETURNING
        VALUE(ref_type) TYPE REF TO cl_abap_datadescr
      RAISING
        cx_dynamic_check.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_path_node,
        path     TYPE string,
        node     TYPE REF TO lcl_tree_node,
        implicit TYPE abap_bool,
      END OF ty_path_node,
      ty_path_nodes TYPE SORTED TABLE OF ty_path_node WITH UNIQUE KEY path.

    CLASS-METHODS build_tree
      IMPORTING
        !rows      TYPE zdot_datadescr
      RETURNING
        VALUE(top_nodes) TYPE ty_nodes.

    CLASS-METHODS build_components
      IMPORTING
        !nodes         TYPE ty_nodes
      RETURNING
        VALUE(comp_tab) TYPE abap_component_tab
      RAISING
        cx_dynamic_check.

    CLASS-METHODS build_elem_descr
      IMPORTING
        !intty TYPE inttype
        !lengt TYPE ilen
        !decim TYPE decimals
      RETURNING
        VALUE(descr) TYPE REF TO cl_abap_datadescr.

ENDCLASS.

CLASS lcl_type_builder IMPLEMENTATION.

  METHOD build.

    DATA(lt_comp) = build_components( build_tree( rows ) ).

    IF lt_comp IS NOT INITIAL.
      DATA(lr_struc) = cl_abap_structdescr=>create( lt_comp ).
      IF type = c_fieldtype_table.
        ref_type = cl_abap_tabledescr=>create( lr_struc ).
      ELSE.
        ref_type = lr_struc.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD build_tree.

    DATA lt_map TYPE ty_path_nodes.

    LOOP AT rows INTO DATA(ls_row).

      SPLIT to_upper( ls_row-fldname ) AT '-' INTO TABLE DATA(lt_segments).
      CHECK lines( lt_segments ) > 0.

      DATA(lv_path) = ``.
      DATA lr_parent TYPE REF TO lcl_tree_node.

      LOOP AT lt_segments INTO DATA(lv_segment).

        IF sy-tabix = 1.
          lv_path = lv_segment.
        ELSE.
          lv_path = |{ lv_path }-{ lv_segment }|.
        ENDIF.

        READ TABLE lt_map ASSIGNING FIELD-SYMBOL(<map>) WITH KEY path = lv_path.
        IF sy-subrc = 0.
          " Path already exists: a later explicit row replaces an
          " implicitly created intermediate node
          IF <map>-implicit = abap_true AND sy-tabix = lines( lt_segments ).
            <map>-node->ms_row = ls_row.
            " the node name is the single segment, not the full path
            <map>-node->ms_row-fldname = lv_segment.
            <map>-implicit = abap_false.
          ENDIF.
          lr_parent = <map>-node.
        ELSE.
          DATA(lr_node) = NEW lcl_tree_node( ).
          DATA(lv_implicit) = boolc( sy-tabix < lines( lt_segments ) ).
          IF lv_implicit = abap_false.
            lr_node->ms_row = ls_row.
          ELSE.
            " Undeclared intermediate segments default to structures
            lr_node->ms_row-fldtype = c_fieldtype_struct.
          ENDIF.
          " the node name is the single segment, not the full path
          lr_node->ms_row-fldname = lv_segment.
          INSERT VALUE #( path = lv_path node = lr_node implicit = lv_implicit )
                 INTO TABLE lt_map.
          IF lr_parent IS BOUND.
            APPEND lr_node TO lr_parent->mt_children.
          ELSE.
            APPEND lr_node TO top_nodes.
          ENDIF.
          lr_parent = lr_node.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD build_components.

    DATA lr_line TYPE REF TO cl_abap_datadescr.

    LOOP AT nodes INTO DATA(lr_node).

      DATA(ls_row) = lr_node->ms_row.

      CASE ls_row-fldtype.
        WHEN c_fieldtype_field.

          IF ls_row-struf IS NOT INITIAL.
            APPEND VALUE #(
              name = ls_row-fldname
              type = CAST cl_abap_datadescr(
                       cl_abap_typedescr=>describe_by_name( ls_row-struf ) ) )
              TO comp_tab.
          ELSEIF ls_row-intty IS NOT INITIAL.
            APPEND VALUE #(
              name = ls_row-fldname
              type = build_elem_descr( intty = ls_row-intty
                                       lengt = ls_row-lengt
                                       decim = ls_row-decim ) )
              TO comp_tab.
          ELSEIF ls_row-refty IS BOUND.
            APPEND VALUE #(
              name = ls_row-fldname
              type = CAST cl_abap_datadescr(
                       cl_abap_typedescr=>describe_by_data_ref( ls_row-refty ) ) )
              TO comp_tab.
          ELSE.
            APPEND VALUE #(
              name = ls_row-fldname
              type = cl_abap_elemdescr=>get_string( ) )
              TO comp_tab.
          ENDIF.

        WHEN c_fieldtype_struct OR c_fieldtype_table.

          IF ls_row-struf IS NOT INITIAL.
            lr_line = CAST cl_abap_datadescr(
                        cl_abap_typedescr=>describe_by_name( ls_row-struf ) ).
          ELSEIF ls_row-intty IS NOT INITIAL.
            lr_line = build_elem_descr( intty = ls_row-intty
                                        lengt = ls_row-lengt
                                        decim = ls_row-decim ).
          ELSE.
            lr_line = cl_abap_structdescr=>create(
                        build_components( lr_node->mt_children ) ).
          ENDIF.

          IF ls_row-fldtype = c_fieldtype_table.
            APPEND VALUE #(
              name = ls_row-fldname
              type = cl_abap_tabledescr=>create( lr_line ) )
              TO comp_tab.
          ELSE.
            APPEND VALUE #(
              name = ls_row-fldname
              type = lr_line )
              TO comp_tab.
          ENDIF.

      ENDCASE.

    ENDLOOP.

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

ENDCLASS.
