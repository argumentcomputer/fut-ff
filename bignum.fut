module type big_params = {
  val size : i32  -- Number of limbs
}

module type bigtype = {
  type t
  type wide

  val size: i32 -- Number of limbs
  val t_bits: i32
  val t_zero: t
  val t_one: t

  val widen: t -> wide
  val narrow: wide -> t

  val add_c: t -> t -> t -> (t, t)
  val mul_c: t -> t -> (t, t)

  val t_equal: t -> t -> bool
  val t_gte: t -> t -> bool
}

module make_big (T: integral) (W: integral) (P: big_params): bigtype = {
  type t = T.t
  type wide = W.t

  let size = P.size
  let t_bits = T.num_bits
  let t_zero = T.highest T.- T.highest
  let t_one = T.highest T./ T.highest

  let widen (x: t): wide = W.i64 (T.to_i64 x)
  let narrow (x: wide): t = T.i64 (W.to_i64 x)

  -- Add three values of type t and return (sum, carry).
  let add_c (a: t) (b: t) (c: t): (t, t) =
    let raw = (widen a) W.+ (widen b) W.+ (widen c) in
    -- Carry bit is either 1 or 0, so this can perhaps be done more cheaply.
    (narrow raw, narrow (raw W.>> W.i32 t_bits))

  -- Multiply two values of type t and return (product, carry).
  let mul_c (a: t) (b: t): (t, t) =
    let raw = (widen a) W.* (widen b) in
    (narrow raw, narrow (raw W.>> W.i32 t_bits))

  let t_equal (a: t) (b: t): bool = a T.== b
  let t_gte (a: t) (b: t): bool = a T.>= b
}

-- In a perfect eventual world, this would include the integral module entirely.
module type arith = {
  type t

  val zero: t
  val one: t
  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val from_u8: u8 -> t

  val ==: t -> t -> bool
  val >=: t -> t -> bool
}

module big (M: bigtype): arith = {
  type t = [M.size]M.t
  let zero: t = map (\_ -> M.t_zero) (iota M.size) -- e.g. [0, 0, 0, 0]
  let one: t = map (\x -> if x == 0 then M.t_one else M.t_zero) (iota M.size) -- e.g. [1, 0, 0, 0]

  let (a: t) == (b: t) : bool = and (map (uncurry M.t_equal) (zip a b))

  let (a: t) >= (b: t) : bool =
    let res = loop (acc, i) = (true, M.size - 1) while acc && (i >= 0) do
                if M.t_gte a[i] b[i] then (true, i - 1) else (false, 0) in
    res.1

  let (a: t) + (b: t) =
    -- res is [(value, carry), ...]
    let res = scan (\acc p -> M.add_c p.1 p.2 acc.2) (M.t_zero, M.t_zero) (zip a b) in
    map (.1) res
  -- FIXME: Detect/handle overflow.

  let (_a: t) -  (_b: t) : t = copy zero -- FIXME: implement
  let (_a: t)* (_b: t) : t = copy zero -- FIXME: implement
  let from_u8 _x: t = copy zero -- FIXME: implement
}

module b32_: bigtype = make_big u8 u16 { let size: i32 = 4}
module b32: arith = big b32_

module b256_: bigtype = (make_big u32 u64 { let size: i32 = 8})
module b256: arith = big b256_

type string = *[]u8


-- Prototype to abstract.
let u64_from_string (s: *[]u8): u64 =
  let parse_digit c = u64.u8 (c - '0') in
  -- Don't use reduce, since, the reduction isn't associative.
  loop acc = 0 for c in s do acc * 10 + parse_digit c

module Stringable(A: arith) = {
  let from_string (s: *[]u8): A.t =
    let parse_digit c: A.t = A.from_u8 (c - '0') in
    let ten = (A.from_u8 10) in
    loop acc = A.zero for c in s do acc A.* ten A.+ (parse_digit c)
}

module a64 = {
  type t = u64
  let zero: t = 0
  let one: t = 1
  let (a: t) + (b: t) : t = a + b
  let (a: t) - (b: t) : t = a - b
  let (a: t) * (b: t) : t = a * b
  let from_u8 x: t = u64.u8 x
  let (a: t) == (b: t) : bool = a == b
  let (a: t) >= (b: t) : bool = a >= b
}

module s64 = Stringable(a64)


let n = s64.from_string("123")
