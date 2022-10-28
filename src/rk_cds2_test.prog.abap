*&---------------------------------------------------------------------*
*& Report RK_CDS2_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT RK_CDS2_TEST.



" added comment
PARAMETERS p_cds type DDLNAME.


data ET_DATASET TYPE ISLM2_T_NATIVEDATASET.
DATA lt_dataset TYPE islm2_t_nativedataset.
 DATA ls_dataset TYPE LINE OF islm2_t_nativedataset.

select * FROM DDCDS_ENTITY_HEADER INTO TABLE @data(lt_header) WHERE entity_name = @p_cds.
select * FROM DDL_OBJECT_NAMES INTO TABLE @data(lt_obj_names) WHERE cds_entity = @p_cds.


************ GET DATASET **********
*
* SELECT CAST( a~cds_entity AS CHAR( 40 ) ) AS tabname,
*       CASE WHEN a~cds_object_type = 'W' THEN a~cds_entity   "For View entity types there is no sqlname hence cds name itself considered.
*           ELSE a~cds_db_view
*           END AS sqlname,
*      'DDLS' AS tabtype,
*      CAST( ' ' AS CHAR( 60 ) ) AS ddtext
*    FROM DDL_OBJECT_NAMES AS a
*    left OUTER JOIN ddheadanno AS b
*    ON a~cds_entity = b~strucobjn AND b~name = @if_islm2_constants=>gc_annot_key_view_type
*    WHERE a~cds_entity = @p_cds and a~state = @if_islm2_constants=>gc_activation_status
*    INTO TABLE @lt_dataset.
*
*
 SELECT CAST( a~cds_entity AS CHAR( 40 ) ) AS tabname,
       CASE WHEN a~cds_object_type = 'W' THEN a~cds_entity   "For View entity types there is no sqlname hence cds name itself considered.
           ELSE a~cds_db_view
           END AS sqlname,
      'DDLS' AS tabtype,
      CAST( ' ' AS CHAR( 60 ) ) AS ddtext
    FROM DDL_OBJECT_NAMES AS a
    WHERE a~cds_entity = @p_cds and a~state = @if_islm2_constants=>gc_activation_status
    INTO TABLE @lt_dataset.  "Add whee clause which filters only type 'V' and 'W'


**********************




***************************** Searh Helps ***********

*
*    SELECT
*    CAST( a~ddlname AS CHAR( 36 ) ) AS guid,
*    a~ddlname  AS name,
*    a~ddlname  AS location-table_name,
*    CAST( ' ' AS CHAR( 60 ) ) AS description,
*    @if_islm2_constants=>gc_dataset AS type,
*     t~devclass AS parent
**the involved tables
*     FROM ddddlsrc AS a
*        INNER JOIN tadir AS t
*        ON t~obj_name =  a~ddlname  AND  t~object = @if_islm2_constants=>gc_ddls AND t~pgmid = @if_islm2_constants=>gc_r3tr
*        LEFT JOIN ddldependency AS e
*        ON a~ddlname = e~ddlname AND e~objecttype = @if_islm2_constants=>gc_view AND e~state = @if_islm2_constants=>gc_activation_status
*        LEFT JOIN dd02l AS f
*        ON e~objectname = f~tabname AND f~as4local = @if_islm2_constants=>gc_activation_status
*        LEFT JOIN p_islm2_released_cds AS r            "#EC CI_BUFFJOIN
*        ON a~ddlname = r~ddl_name
*     WHERE a~ddlname IN @it_name_seltab AND
*           a~as4local = @if_islm2_constants=>gc_activation_status AND
*           f~with_parameters NE @abap_true AND
*           (
*            " SAP Objects ( + Partners )
*             ( NOT ( upper( left( a~ddlname, 1 ) ) = 'Y' OR upper( left( a~ddlname, 1 ) ) = 'Z' ) AND
*               ( @iv_cds_released_sap = @abap_false OR
*                 ( @iv_cds_released_sap = @abap_true AND r~ddl_name IS NOT NULL  )
*               )
*             ) OR
*             " Customer Object
*             ( ( upper( left( a~ddlname, 1 ) ) = 'Y' OR upper( left( a~ddlname, 1 ) ) = 'Z' ) AND
*               ( @iv_cds_released_nonsap = @abap_false OR
*                 ( @iv_cds_released_nonsap = @abap_true AND r~ddl_name IS NOT NULL  )
*               )
*             )
*           )
*     ORDER BY a~ddlname                           " Sort at the DB level since the CDS names are in upper cases
*     INTO CORRESPONDING FIELDS OF TABLE @et_datasets.
*

data : IV_CDS_RELEASED_SAP type abap_bool VALUE ' '.
data : IV_CDS_RELEASED_NONSAP type abap_bool VALUE ''.
data it_name_seltab TYPE ISLM_T_SELECT_OPTION.


   APPEND value #( sign = 'I' option = 'EQ' low = p_cds ) to it_name_seltab.

data et_searchhelp type ISLM2_T_DATASET.

 SELECT
    CAST( a~entity_name AS CHAR( 36 ) ) AS guid,
    a~ddl_name  AS name,
    a~ddl_name  AS location-table_name,
    CAST( ' ' AS CHAR( 60 ) ) AS description,
    @if_islm2_constants=>gc_dataset AS type,
     t~devclass AS parent
     FROM DDCDS_ENTITY_HEADER AS a
        INNER JOIN tadir AS t
        ON t~obj_name =  a~ddl_name  AND  t~object = @if_islm2_constants=>gc_ddls AND t~pgmid = @if_islm2_constants=>gc_r3tr
       LEFT JOIN ZRK_P_ISLM2_RELEASED_CDS AS r
         ON a~ddl_name = r~ddl_name
     WHERE a~ddl_name IN @it_name_seltab AND
           a~state = @if_islm2_constants=>gc_activation_status and
           a~entity_has_parameters NE @abap_true AND
          (
*             SAP Objects ( + Partners )
             ( NOT ( upper( left( a~ddl_name, 1 ) ) = 'Y' OR upper( left( a~ddl_name, 1 ) ) = 'Z' ) AND
               ( @iv_cds_released_sap = @abap_false OR
                 ( @iv_cds_released_sap = @abap_true AND r~ddl_name IS NOT NULL  )
               )
             ) OR
**              Customer Object
             ( ( upper( left( a~ddl_name, 1 ) ) = 'Y' OR upper( left( a~ddl_name, 1 ) ) = 'Z' ) AND
               ( @iv_cds_released_nonsap = @abap_false OR
                 ( @iv_cds_released_nonsap = @abap_true AND r~ddl_name IS NOT NULL  )
               )
             )
           )
     ORDER BY a~ddl_name                           " Sort at the DB level since the CDS names are in upper cases
     INTO CORRESPONDING FIELDS OF TABLE @et_searchhelp.









**************************

data(out) = cl_demo_output=>new( )->write_data( lt_header ).
out->write_data( lt_obj_names ).
out->write_data( lt_dataset ).
out->write_data( et_searchhelp ).
out->display( ).
