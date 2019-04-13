  module FromOCamlToReason :
  sig
    val transform : string list -> string list option
  end

module FromReasonToOCaml :
  sig
    val transform : string list -> string list option
  end