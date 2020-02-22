module type field_params = {
  val size : i32  -- Number of limbs
}

module type fieldtype = {
  type t

  val size: i32 -- Number of limbs
  val t_bits: i32
  val t_zero: t
  val t_one: t
  val t_highest: t

  val t_equal: t -> t -> bool
  val t_gt: t -> t -> bool
  val t_gte: t -> t -> bool
  val t_lt: t -> t -> bool
  val t_lte: t -> t -> bool
  val t_add: t -> t -> t
  val t_sub: t -> t -> t
  val t_from_u8: u8 -> t
}

module make_field (T: integral) (P: field_params): fieldtype = {
  type t = T.t

  let size = P.size
  let t_bits = T.num_bits
  let t_zero = T.highest T.- T.highest
  let t_one = T.highest T./ T.highest
  let t_highest = T.highest

  let t_equal (a: t) (b: t): bool = a T.== b
  let t_gt (a: t) (b: t): bool = a T.> b
  let t_gte (a: t) (b: t): bool = a T.>= b
  let t_lt (a: t) (b: t): bool = a T.< b
  let t_lte (a: t) (b: t): bool = a T.<= b
  let t_add (a: t) (b: t): t = a T.+ b
  let t_sub (a: t) (b: t): t = a T.- b
  let t_from_u8 (n: u8): t = T.u8 n
}

-- In a perfect eventual world, this would include the integral module entirely.
module type field = {
  type t
  type s -- limb type

  val zero: t
  val one: t
  val x: t
  val xx: t
  val xxx: t
  val highest: t
  val fill: s -> i32 -> t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t

  val ==: t -> t -> bool
  val >=: t -> t -> bool

  val from_u8: u8 -> t
  val from_string: (s: *[]u8) -> t
}

module big_field (M: fieldtype): field = {
  type t = [M.size]M.t
  type s = M.t

  let zero: t = map (\_ -> M.t_zero) (iota M.size) -- e.g. [0, 0, 0, 0]
  let one: t = map (\x -> if x == 0 then M.t_one else M.t_zero) (iota M.size) -- e.g. [1, 0, 0, 0]
  let x: t = map (\x -> if x < 1 then M.t_highest else M.t_zero) (iota M.size) -- e.g. [-1, 0, 0, 0]
  let xx: t = map (\x -> if x < 2 then M.t_highest else M.t_zero) (iota M.size) -- e.g. [-1, -1, 0, 0]
  let xxx: t = map (\x -> if x < 3 then M.t_highest else M.t_zero) (iota M.size) -- e.g. [-1, -1, -1, 0]
  let highest: t = map (const M.t_highest) (iota M.size) -- e.g. [-1, -1, -1, -1]

--  let fill (v: t) (count: t) = map (\x -> if x < count then v else M.t_zero) (iota M.size)
  let fill (v: M.t) (count: i32) : t   = map (\x -> if x < count then v else M.t_zero) (iota M.size)

  let (a: t) == (b: t) : bool = and (map (uncurry M.t_equal) (zip a b))

  let (a: t) >= (b: t) : bool =
    let res = loop (acc, i) = (true, M.size - 1) while acc && (i >= 0) do
                if M.t_gte a[i] b[i] then (true, i - 1) else (false, 0) in
    res.1

  let (a: t) + (b: t) =
    let (_, r) = loop (carry, r) = (M.t_zero, []) for i < M.size do
      let old = a[i] in
      let tmp = M.t_add a[i] (M.t_add b[i] carry) in
      let carry =
        if (M.t_gt carry M.t_zero) then
        if (M.t_gte old tmp) then M.t_one else M.t_zero
        else
        if M.t_gt old tmp then M.t_one else M.t_zero
      in
      (carry, r ++ [tmp]) in
    r

  let (a: t) -  (b: t) : t =
    let (_, r) = loop (borrow, r) = (M.t_zero, []) for i < M.size do
      let old = a[i] in
      let tmp = M.t_sub a[i] (M.t_add b[i] borrow) in
      let borrow =
        if (M.t_gt borrow M.t_zero) then
        if (M.t_lte old tmp) then M.t_one else M.t_zero
        else
        if M.t_lt old tmp then M.t_one else M.t_zero
      in
      (borrow, r ++ [tmp]) in
    r

  let (_a: t)* (_b: t) : t = copy zero -- FIXME: implement
  let from_u8 (n: u8): t = fill (M.t_from_u8 n) 1

  let from_string (s: *[]u8): t =
    let parse_digit (c: u8): t = from_u8 (c u8.- '0') in
    let ten = (from_u8 10) in
    loop acc = zero for c in s do acc * ten + (parse_digit c)
}

module b32_: fieldtype = make_field u8 { let size: i32 = 4}
module b32: field = big_field b32_

module b256_: fieldtype = (make_field u64 { let size: i32 = 4})
module b256: field = big_field b256_

type string = *[]u8


-- Prototype to abstract.
let u64_from_string (s: *[]u8): u64 =
  let parse_digit c = u64.u8 (c - '0') in
  -- Don't use reduce, since, the reduction isn't associative.
  loop acc = 0 for c in s do acc * 10 + parse_digit c

-- module Stringable(A: field) = {
--   let from_string (s: *[]u8): A.t =
--     let parse_digit c: A.t = A.from_u8 (c - '0') in
--     let ten = (A.from_u8 10) in
--     loop acc = A.zero for c in s do acc A.* ten A.+ (parse_digit c)
-- }

-- module a64 = {
--   type t = u64
--   type s = u64

--   let zero: t = 0
--   let one: t = 1
--   let x: t = u64.highest
--   let xx: t = u64.highest
--   let xxx: t = u64.highest
--   let highest: t = u64.highest
--   let fill (v: t) (_count: i32) : t = v

--   let (a: t) + (b: t) : t = a + b
--   let (a: t) - (b: t) : t = a - b
--   let (a: t) * (b: t) : t = a * b
--   let from_u8 x: t = u64.u8 x
--   let (a: t) == (b: t) : bool = a == b
--   let (a: t) >= (b: t) : bool = a >= b
-- }

-- module s64 = Stringable(a64)
-- module s32 = Stringable(b32)

let n = b32.from_string("123")
