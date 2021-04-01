include module type of struct
  include StringLabels
end

module Map : Map.S with type key = string

val starts_with : prefix:string -> string -> bool
