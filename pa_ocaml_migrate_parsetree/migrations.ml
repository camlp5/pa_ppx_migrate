(**pp -syntax camlp5o *)

module type ASTSIG = sig
  type location_t
  type expression
  type core_type
  type structure_item
  type signature_item
  type type_extension
end

module type RAW_MIGRATION = sig
  module SRC : ASTSIG
  module DST : ASTSIG
  type 'aux dispatch_table_t
  type ('aux, 'a, 'b) migrater_t =
    'aux dispatch_table_t -> SRC.location_t option -> 'a -> 'b
  val make_dt : 'a -> 'a dispatch_table_t
  val migrate_expression :
    'aux .
    ('aux, SRC.expression, DST.expression)
      migrater_t
  val migrate_core_type :
    'aux .
    ('aux, SRC.core_type, DST.core_type)
      migrater_t
  val migrate_structure_item :
    'aux .
    ('aux, SRC.structure_item, DST.structure_item)
      migrater_t
  val migrate_signature_item :
    'aux .
    ('aux, SRC.signature_item, DST.signature_item)
      migrater_t
  val migrate_type_extension :
    'aux .
    ('aux, SRC.type_extension, DST.type_extension)
      migrater_t
end

module type MIGRATION = sig
  module SRC : ASTSIG
  module DST : ASTSIG
  val expression : SRC.expression -> DST.expression
  val core_type : SRC.core_type -> DST.core_type
  val structure_item : SRC.structure_item -> DST.structure_item
  val signature_item : SRC.signature_item -> DST.signature_item
  val type_extension : SRC.type_extension -> DST.type_extension
end

module  Migrate
          (M : RAW_MIGRATION)
 : (MIGRATION with module SRC = M.SRC and module DST = M.DST) =
  struct
    module SRC = M.SRC
    module DST = M.DST
    let expression e = M.migrate_expression (M.make_dt()) None e
    let core_type e = M.migrate_core_type (M.make_dt()) None e
    let structure_item e = M.migrate_structure_item (M.make_dt()) None e
    let signature_item e = M.migrate_signature_item (M.make_dt()) None e
    let type_extension e = M.migrate_type_extension (M.make_dt()) None e
  end

module Compose
         (M1 : MIGRATION)
         (M2:MIGRATION with module SRC = M1.DST)
       : (MIGRATION with module SRC = M1.SRC and module DST = M2.DST) =
  struct
    module SRC = M1.SRC
    module DST = M2.DST
    let expression e = e |> M1.expression |> M2.expression
    let core_type e = e |> M1.core_type |> M2.core_type
    let structure_item e = e |> M1.structure_item |> M2.structure_item
    let signature_item e = e |> M1.signature_item |> M2.signature_item
    let type_extension e = e |> M1.type_extension |> M2.type_extension
  end

module Migrate_402_403 = Migrate(Migrate_402_403)
module Migrate_402_404 = Migrate(Migrate_402_404)
module Migrate_402_405 = Migrate(Migrate_402_405)
module Migrate_402_406 = Migrate(Migrate_402_406)
module Migrate_402_407 = Migrate(Migrate_402_407)
module Migrate_403_402 = Migrate(Migrate_403_402)
module Migrate_403_404 = Migrate(Migrate_403_404)
module Migrate_404_402 = Migrate(Migrate_404_402)
module Migrate_404_403 = Migrate(Migrate_404_403)
module Migrate_404_405 = Migrate(Migrate_404_405)
module Migrate_405_402 = Migrate(Migrate_405_402)
module Migrate_405_404 = Migrate(Migrate_405_404)
module Migrate_405_406 = Migrate(Migrate_405_406)
module Migrate_406_402 = Migrate(Migrate_406_402)
module Migrate_406_405 = Migrate(Migrate_406_405)
module Migrate_406_407 = Migrate(Migrate_406_407)
module Migrate_407_402 = Migrate(Migrate_407_402)
module Migrate_407_406 = Migrate(Migrate_407_406)
module Migrate_407_408 = Migrate(Migrate_407_408)
module Migrate_408_407 = Migrate(Migrate_408_407)
module Migrate_408_409 = Migrate(Migrate_408_409)
module Migrate_409_408 = Migrate(Migrate_409_408)
module Migrate_409_410 = Migrate(Migrate_409_410)
module Migrate_410_409 = Migrate(Migrate_410_409)
module Migrate_410_411 = Migrate(Migrate_410_411)
module Migrate_411_410 = Migrate(Migrate_411_410)
module Migrate_411_412 = Migrate(Migrate_411_412)
module Migrate_412_411 = Migrate(Migrate_412_411)
module Migrate_412_413 = Migrate(Migrate_412_413)
module Migrate_413_412 = Migrate(Migrate_413_412)
module Migrate_413_414 = Migrate(Migrate_413_414)
module Migrate_414_413 = Migrate(Migrate_414_413)
module Migrate_414_500 = Migrate(Migrate_414_500)
module Migrate_500_414 = Migrate(Migrate_500_414)

module Migrate_413_500 = Compose(Migrate_413_414)(Migrate_414_500)
module Migrate_500_413 = Compose(Migrate_500_414)(Migrate_414_413)

module Migrate_412_500 = Compose(Migrate_412_413)(Migrate_413_500)
module Migrate_500_412 = Compose(Migrate_500_413)(Migrate_413_412)

module Migrate_411_500 = Compose(Migrate_411_412)(Migrate_412_500)
module Migrate_500_411 = Compose(Migrate_500_412)(Migrate_412_411)

module Migrate_410_500 = Compose(Migrate_410_411)(Migrate_411_500)
module Migrate_500_410 = Compose(Migrate_500_411)(Migrate_411_410)
