type buffer = string

module type Codec = sig
  type t

  val encode : t -> buffer
  val decode : buffer -> t
end

module type S = sig
  module Request : sig
    type t

    include Codec with type t := t
  end

  module Response : sig
    type t

    include Codec with type t := t
  end

  val package_name : string option
  val service_name : string
  val method_name : string
end
