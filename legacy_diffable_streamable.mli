module Make (T : sig
  type t

  include Legacy_diffable_intf.S with type t := t
end) : sig
  include Streamable.S with type t := T.t
end

module Make_rpc (T : sig
  type t

  include Legacy_diffable_intf.S_plain with type t := t
end)
(Diff : sig
  type t [@@deriving bin_io]
end
with type t = T.Update.Diff.t) : sig
  include Streamable.S_rpc with type t := T.t
end
