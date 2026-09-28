CLASS zcx_dynamic_json_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA text TYPE string READ-ONLY .

    METHODS constructor
      IMPORTING
        !i_text TYPE string OPTIONAL .

ENDCLASS.



CLASS zcx_dynamic_json_error IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    text = i_text.

  ENDMETHOD.
ENDCLASS.
