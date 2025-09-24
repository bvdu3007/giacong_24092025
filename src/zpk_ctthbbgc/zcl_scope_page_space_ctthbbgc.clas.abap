CLASS zcl_scope_page_space_ctthbbgc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SCOPE_PAGE_SPACE_CTTHBBGC IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.


    DATA(lo_scope_api) = cl_aps_bc_scope_change_api=>create_instance( ).

    lo_scope_api->scope(
    EXPORTING it_object_scope = VALUE #(
    pgmid = if_aps_bc_scope_change_api=>gc_tadir_pgmid-r3tr
    scope_state = if_aps_bc_scope_change_api=>gc_scope_state-on

* Space template
   ( object = if_aps_bc_scope_change_api=>gc_tadir_object-uist obj_name = 'Z_CTTHBBGC' )

* Page template
    ( object = if_aps_bc_scope_change_api=>gc_tadir_object-uipg obj_name = 'Z_CTTHBBGC' )
    )

            iv_simulate = abap_false
            iv_force = abap_false
    IMPORTING et_object_result = DATA(lt_results)
            et_message = DATA(lt_messages) ).
  ENDMETHOD.
ENDCLASS.
