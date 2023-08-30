module Make2 (A : Diffable_intf.S) (B : Diffable_intf.S) :
  Diffable_intf.S with type t = A.t * B.t

module Make2_plain (A : Diffable_intf.S_plain) (B : Diffable_intf.S_plain) :
  Diffable_intf.S_plain with type t = A.t * B.t

module Make3 (A : Diffable_intf.S) (B : Diffable_intf.S) (C : Diffable_intf.S) :
  Diffable_intf.S with type t = A.t * B.t * C.t

module Make3_plain
  (A : Diffable_intf.S_plain)
  (B : Diffable_intf.S_plain)
  (C : Diffable_intf.S_plain) : Diffable_intf.S_plain with type t = A.t * B.t * C.t

module Make4
  (A : Diffable_intf.S)
  (B : Diffable_intf.S)
  (C : Diffable_intf.S)
  (D : Diffable_intf.S) : Diffable_intf.S with type t = A.t * B.t * C.t * D.t

module Make4_plain
  (A : Diffable_intf.S_plain)
  (B : Diffable_intf.S_plain)
  (C : Diffable_intf.S_plain)
  (D : Diffable_intf.S_plain) : Diffable_intf.S_plain with type t = A.t * B.t * C.t * D.t

module Make5
  (A : Diffable_intf.S)
  (B : Diffable_intf.S)
  (C : Diffable_intf.S)
  (D : Diffable_intf.S)
  (E : Diffable_intf.S) : Diffable_intf.S with type t = A.t * B.t * C.t * D.t * E.t

module Make5_plain
  (A : Diffable_intf.S_plain)
  (B : Diffable_intf.S_plain)
  (C : Diffable_intf.S_plain)
  (D : Diffable_intf.S_plain)
  (E : Diffable_intf.S_plain) :
  Diffable_intf.S_plain with type t = A.t * B.t * C.t * D.t * E.t

module Make6
  (A : Diffable_intf.S)
  (B : Diffable_intf.S)
  (C : Diffable_intf.S)
  (D : Diffable_intf.S)
  (E : Diffable_intf.S)
  (F : Diffable_intf.S) : Diffable_intf.S with type t = A.t * B.t * C.t * D.t * E.t * F.t

module Make6_plain
  (A : Diffable_intf.S_plain)
  (B : Diffable_intf.S_plain)
  (C : Diffable_intf.S_plain)
  (D : Diffable_intf.S_plain)
  (E : Diffable_intf.S_plain)
  (F : Diffable_intf.S_plain) :
  Diffable_intf.S_plain with type t = A.t * B.t * C.t * D.t * E.t * F.t
