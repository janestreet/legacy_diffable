module Make2 (A : Legacy_diffable_intf.S) (B : Legacy_diffable_intf.S) :
  Legacy_diffable_intf.S with type t = A.t * B.t

module Make2_plain (A : Legacy_diffable_intf.S_plain) (B : Legacy_diffable_intf.S_plain) :
  Legacy_diffable_intf.S_plain with type t = A.t * B.t

module Make3
    (A : Legacy_diffable_intf.S)
    (B : Legacy_diffable_intf.S)
    (C : Legacy_diffable_intf.S) : Legacy_diffable_intf.S with type t = A.t * B.t * C.t

module Make3_plain
    (A : Legacy_diffable_intf.S_plain)
    (B : Legacy_diffable_intf.S_plain)
    (C : Legacy_diffable_intf.S_plain) :
  Legacy_diffable_intf.S_plain with type t = A.t * B.t * C.t

module Make4
    (A : Legacy_diffable_intf.S)
    (B : Legacy_diffable_intf.S)
    (C : Legacy_diffable_intf.S)
    (D : Legacy_diffable_intf.S) :
  Legacy_diffable_intf.S with type t = A.t * B.t * C.t * D.t

module Make4_plain
    (A : Legacy_diffable_intf.S_plain)
    (B : Legacy_diffable_intf.S_plain)
    (C : Legacy_diffable_intf.S_plain)
    (D : Legacy_diffable_intf.S_plain) :
  Legacy_diffable_intf.S_plain with type t = A.t * B.t * C.t * D.t

module Make5
    (A : Legacy_diffable_intf.S)
    (B : Legacy_diffable_intf.S)
    (C : Legacy_diffable_intf.S)
    (D : Legacy_diffable_intf.S)
    (E : Legacy_diffable_intf.S) :
  Legacy_diffable_intf.S with type t = A.t * B.t * C.t * D.t * E.t

module Make5_plain
    (A : Legacy_diffable_intf.S_plain)
    (B : Legacy_diffable_intf.S_plain)
    (C : Legacy_diffable_intf.S_plain)
    (D : Legacy_diffable_intf.S_plain)
    (E : Legacy_diffable_intf.S_plain) :
  Legacy_diffable_intf.S_plain with type t = A.t * B.t * C.t * D.t * E.t

module Make6
    (A : Legacy_diffable_intf.S)
    (B : Legacy_diffable_intf.S)
    (C : Legacy_diffable_intf.S)
    (D : Legacy_diffable_intf.S)
    (E : Legacy_diffable_intf.S)
    (F : Legacy_diffable_intf.S) :
  Legacy_diffable_intf.S with type t = A.t * B.t * C.t * D.t * E.t * F.t

module Make6_plain
    (A : Legacy_diffable_intf.S_plain)
    (B : Legacy_diffable_intf.S_plain)
    (C : Legacy_diffable_intf.S_plain)
    (D : Legacy_diffable_intf.S_plain)
    (E : Legacy_diffable_intf.S_plain)
    (F : Legacy_diffable_intf.S_plain) :
  Legacy_diffable_intf.S_plain with type t = A.t * B.t * C.t * D.t * E.t * F.t
