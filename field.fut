module type field_params = {
  val limbs : i32  -- Number of limbs
}

module type fieldtype = {
  type t

  val limbs: i32 -- number of limbs
  val double_limbs: i32 -- 2 x number of limbs
  val num_bits: i32
  val zero: t
  val one: t
  val highest: t

  val equal: t -> t -> bool
  val gt: t -> t -> bool
  val gte: t -> t -> bool
  val lt: t -> t -> bool
  val lte: t -> t -> bool
  val add: t -> t -> t
  val mul: t -> t -> t
  val sub: t -> t -> t
  val from_u8: u8 -> t
  val mad_hi: t -> t -> t -> t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t

  val ==: t -> t -> bool
  val >=: t -> t -> bool
  val >: t -> t -> bool
  val <: t -> t -> bool
  val <=: t -> t -> bool

  val >>: t -> i32 -> t
  val <<: t -> i32 -> t
  val |: t -> t -> t
}

module make_field (T: integral) (P: field_params): fieldtype = {
  type t = T.t

  let limbs = P.limbs
  let double_limbs = 2 * P.limbs
  let num_bits = T.num_bits
  let zero = T.highest T.- T.highest
  let one = T.highest T./ T.highest
  let highest = T.highest

  let equal (a: t) (b: t): bool = a T.== b
  let gt (a: t) (b: t): bool = a T.> b
  let gte (a: t) (b: t): bool = a T.>= b
  let lt (a: t) (b: t): bool = a T.< b
  let lte (a: t) (b: t): bool = a T.<= b
  let add (a: t) (b: t): t = a T.+ b
  let sub (a: t) (b: t): t = a T.- b
  let mul (a: t) (b: t): t = a T.* b
  let from_u8 (n: u8): t = T.u8 n
  let mad_hi (a: t) (b: t) (c: t): t = T.mad_hi a b c

  let (a: t) + (b: t) = a T.+ b
  let (a: t) - (b: t) = a T.- b
  let (a: t) * (b: t) = a T.* b
  let (a: t) == (b: t) = a T.== b
  let (a: t) >= (b: t) = a T.>= b
  let (a: t) > (b: t) = a T.> b
  let (a: t) <= (b: t) = a T.<= b
  let (a: t) < (b: t) = a T.< b
  let (x: t) >> (n: i32) = x T.>> (T.i32 n)
  let (x: t) << (n: i32) = x T.<< (T.i32 n)
  let (a: t) | (b: t) = a T.| b
}

type^ field_core 't 's = ((u8->t), t, t->t->t, t->t->t, u8->s)

-- In a perfect eventual world, this would include the integral module entirely.
module type field = {
  type t
  type s -- limb type
  type double_t

  val zero: t
  val one: t
  val highest: t
  val fill: s -> i32 -> t


  -- Normal operations
  val add: *t -> t -> t
  val sub: *t -> t -> t

  val ==: t -> t -> bool
  val >=: t -> t -> bool
  val >: t -> t -> bool
  val <=: t -> t -> bool
  val <: t -> t -> bool

  val big_mul: *t -> t -> double_t

  -- Field operations
  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t

  val to_mont: t -> t
  val mont_reduce: *double_t -> t
  val mont_field_mul: t -> t -> t
  val final_reduce: t -> t

  val double: t -> t
  val square: t -> t
  val pow: t -> i32 -> t
  val pow_lookup: t -> i32 -> t

  val from_u8: u8 -> t
  val mont_u8: u8 -> t
  val core : field_core t s

  val mac_with_carry: s -> s -> s -> s -> (s, s)
  val mac_with_carry8: u8 -> u8 -> u8 -> u8 -> (s, s)
  val add_with_carry: s -> s -> (s, s)
  val add2_with_carry: s -> s -> s -> (s, s)

  val FIELD_P: t
  val FIELD_INV: s
}

module big_field (M: fieldtype): field = {
  type t = [M.limbs]M.t
  type s = M.t
  type double_t = [M.double_limbs]s

  let limbs_are_unsigned = assert (M.lt M.zero (M.sub M.zero M.one)) true

  let zero: t = map (\_ -> M.zero) (iota M.limbs) -- e.g. [0, 0, 0, 0]
  let one: t = map (\x -> if x == 0 then M.one else M.zero) (iota M.limbs) -- e.g. [1, 0, 0, 0]
  let highest: t = map (const M.highest) (iota M.limbs) -- e.g. [-1, -1, -1, -1]

  let dummy = M.(from_u8 251)
--  let FIELD_P = map (\i -> if i < M.limbs then M.highest else M.zero) (iota M.limbs) -- FIXME
  let FIELD_P = zero -- bignum case.

  let DOUBLE_FIELD_P: double_t =
    let fp = copy FIELD_P in
    map (\i -> if i < M.limbs then fp[i] else M.zero) (iota M.double_limbs)

  let calc_inv (a: s): s =
    let inv = (loop inv = M.one for _i < M.num_bits do
               let inv = M.(inv * inv) in M.(inv * a))
    in M.(zero - inv)

  let FIELD_INV = calc_inv FIELD_P[0] 

  let fill (v: s) (count: i32) : t = map (\x -> if x < count then v else M.zero) (iota M.limbs)

  let (a: t) == (b: t) : bool = and (map (uncurry M.equal) (zip a b))

  let bool_t (cond: bool): s = if cond then M.one else M.zero

  let (a: t) >= (b: t) : bool =
    let res = loop (acc, i) = (true, M.limbs - 1) while acc && (i >= 0) do
                if M.(a[i] >= b[i]) then (true, i - 1) else (false, 0) in
    res.0

  let (a: t) > (b: t) : bool =
      let res = loop (acc, i) = (true, M.limbs - 1) while acc && (i i32.>= 0) do
                if M.(a[i] > b[i]) then (true, i - 1) else (false, 0) in
    res.0

  let (a: t) <= (b: t) : bool = b > a

  let (a: t) < (b: t) : bool = b >= a

  let add (a: t) (b: t) =
    let (_, r) = loop (carry, r) = (M.zero, []) for i < M.limbs do
      let old = a[i] in
      let tmp = M.(a[i] + b[i] + carry) in
      let carry = if M.(carry > zero) then
                    bool_t M.(old > tmp)
                  else
                    bool_t M.(old > tmp) in
      (carry, r ++ [tmp]) in
    r

--  let p_in_range = assert ((add FIELD_P FIELD_P) < FIELD_P) true

  let sub (a: t) (b: t) : t =
    let (_, r) = loop (borrow, r) = (M.zero, []) for i < M.limbs do
      let old = a[i] in
      let tmp = M.(a[i] - (b[i] + borrow)) in
      let borrow = if M.(borrow > zero) then
                     bool_t M.(old <= tmp)
                   else
                     bool_t M.(old < tmp) in
      (borrow, r ++ [tmp]) in
    r

  -- TODO: try to make this generic over size.
  let double_sub (a: double_t) (b: double_t) : double_t =
    let (_, r) = loop (borrow, r) = (M.zero, []) for i < M.double_limbs do
      let old = a[i] in
      let tmp = M.(a[i] - (b[i] + borrow)) in
      let borrow = if M.(borrow > zero) then
                     bool_t M.(old <= tmp)
                   else
                     bool_t M.(old < tmp) in
      (borrow, r ++ [tmp]) in
    r

  let mac_with_carry (a: s) (b: s) (c: s) (d: s): (s, s) =
    let lo = M.(a * b + c) in
    let hi =  M.(mad_hi a b (bool_t (lo < c))) in
    let a = lo in
    let lo = M.(lo + d) in
    let hi = M.(hi + (bool_t (lo < a))) in
    let d = hi in
    (lo, d)

    -- For testing
    let mac_with_carry8 (a: u8) (b: u8) (c: u8) (d: u8): (s, s) =
      M.(mac_with_carry (from_u8 a) (from_u8 b) (from_u8 c) (from_u8 d))

  let add_with_carry (a: s) (b: s): (s, s) =
    let lo = M.(a + b) in
    let b = bool_t M.(lo < a) in
    (lo, b)

  let add2_with_carry (a: s) (b: s) (c: s): (s, s) =
    let lo = M.(a + b) in
    let hi = bool_t M.(lo < a) in
    let a = lo in
    let lo = M.(lo + c) in
    let hi = M.(hi + (bool_t (lo < a))) in
    let c = hi in
    (lo, c)

  let mont_reduce (limbs: *double_t): t =
    let (FIELD_P, FIELD_INV) = (copy (FIELD_P, FIELD_INV)) in
    let carry2 = M.zero in
    let (outer, _) = loop (outer, carry2) = (limbs, carry2) for i < M.limbs do
               let u = M.(FIELD_INV * outer[i]) in
               let carry = M.zero in
               let (inner1, carry) = loop (inner, carry) = (outer, carry) for j < M.limbs do
                         let box = inner[i + j] in
                         let (x, carry) = mac_with_carry u FIELD_P[j] (copy box) carry in
                         (inner with [i + j] = x, carry) in
               let box = [inner1[i + M.limbs]] in
               let (x, carry2) = add2_with_carry box[0] carry carry2 in
               (inner1 with [i + M.limbs] = x, carry2)  in
    let result: *t = map (\i -> outer[i + M.limbs]) (iota M.limbs) in
    if result >= FIELD_P then
      sub result FIELD_P
    else result

  let final_reduce (v: t): t =
    let double = map (\i -> if i i32.< M.limbs then v[i] else M.zero) (iota M.double_limbs) in
    mont_reduce double

    -- Is double-width a in the field?
    let double_in_field (a: double_t): bool =
      if FIELD_P == zero then true else
      all (\i -> M.(a[i i32.+ limbs] == zero)) (iota M.limbs) &&
       let res = loop (acc, i) = (true, M.limbs - 1) while acc && (i i32.>= 0) do
                 if M.(a[i] < FIELD_P[i]) then (true, i - 1) else (false, 0) in
       res.0

  let simple_reduce (to_reduce: double_t): t =
    let dfp = (copy DOUBLE_FIELD_P) in
    let in_field = loop to_reduce = to_reduce while !(double_in_field to_reduce) do
      double_sub to_reduce dfp in
    map (\i -> in_field[i]) (iota M.limbs)

  let big_mul (a: []M.t) (b: t): []M.t =
    let (res: *double_t) = map (const M.zero) (iota M.double_limbs) in
    let (outer, _) =
      loop (outer, i) = (res, 0) while i i32.< M.limbs do
      let (inner, carry) =
        (loop (inner, carry) = (outer, M.zero) for j < M.limbs do
         let box = inner[i + j] in
         let (sum, carry) = mac_with_carry a[i] b[j] (copy box) carry in
         (inner with [i + j] = sum, carry)) in
      (inner with [i + M.limbs] = carry, i i32.+ 1) in
    outer

  let R_MOD_P = let z: *t = (copy zero) in sub z FIELD_P
  let to_mont(v: t): t =
    simple_reduce (big_mul (copy R_MOD_P) v)

  let simple_field_mul  (a: t) (b: t) : t =
    simple_reduce (big_mul a b)

  let mont_field_mul  (a: t) (b: t) : t =
    mont_reduce (big_mul a b)


  let (a: t) * (b: t) : t =
    simple_field_mul a b

  let (a: t) - (b: t): t =
    let old = copy a in
    let res = sub a b in
    if old >= b then res else add res (copy FIELD_P)

  let (a: t) + (b: t): t =
    let res = add a b in
    if FIELD_P == zero then res else -- special case for simple bignum (make own module?)
    let fp = (copy FIELD_P) in
    if res >= fp then sub res fp else res

  let square (x: t): t =
    let (res: *double_t) = map (const M.zero) (iota M.double_limbs) in
    let res =
      (loop (outer) = res for i < M.limbs do
      let (inner, carry, _) =
        (loop (inner, carry, j) = (outer, M.zero, i i32.+ 1) while j i32.< M.limbs do
         let box = inner[i i32.+ j] in
         let (sum, carry) = mac_with_carry x[i] x[j] (copy box) carry in
         (inner with [i i32.+ j] = sum, carry, j i32.+ 1)) in
      inner with [i i32.+ M.limbs] = carry) in
    let box = res[i32.(M.limbs * 2 - 2)] in
    let res = res with [i32.(M.limbs * 2 - 1)] = (copy box) M.>> (i32.(M.limbs - 1)) in

    let (res, _) = loop (res, i) = (res, i32.(M.limbs * 2 - 2)) while (i i32.> 1) do
                   let box = res[i] in
                   let box2 = res[i i32.- 1] in
                   let res = res with [i] = (copy box) M.<< 1 M.| ((copy box2) M.>> 1) in
                   (res, i i32.+ 1) in
    let (_, _, res) = loop (i, carry, res) = (0, M.zero, res) while i i32.< M.limbs do
              let box = res[i i32.* 2] in
              let (x, carry) = mac_with_carry x[i] x[i] (copy box) carry in
              let box2 = res[i32.(i * 2 + 1)] in
              let (y, carry) = add_with_carry (copy box2) carry in
              let res = res with [i32.(i * 2)] = x in
              let res = res with [i32.(i * 2 + 1)] = y in
              (i i32.+ 1, carry, res) in
    simple_reduce res

  let double (_x: t): t = assert false (copy zero) -- FIXME: implement
  let pow (_base: t) (_exp: i32): t = assert false (copy zero) -- FIXME: implement
  let pow_lookup (_base: t) (_exp: i32): t = assert false (copy zero) -- FIXME: implement

  let from_u8 (n: u8): t = fill (M.from_u8 n) 1
  let mont_u8 (n: u8): t = to_mont (from_u8 n)
  let s_from_u8 (n: u8): s = M.from_u8 n

  let core: field_core t s = (from_u8, zero, (*), (+), s_from_u8)
}

-- This is conceptually a function of the field module, but the anonymous limbs parameter (s)
-- leads to compile errors in the module type. So use just the vals we need, encapsulated as field_core.
let from_string 't 's (core: field_core t s) (str: *[]u8): t =
  let (from_u8, zero, mul, add, _) = core in
  let ten = (from_u8 10) in
  let parse_digit (c: u8): t = from_u8 (c u8.- '0') in
  loop acc = zero for c in str do add (mul acc ten) (parse_digit c)

module b32: field = big_field (make_field u8 { let limbs: i32 = 4})
let b32_from_string (s: *[]u8) = from_string b32.core s

module b24: field = big_field (make_field u8 { let limbs: i32 = 3})
let b24_from_string (s: *[]u8) = from_string b24.core s

module b256: field = big_field (make_field u64 { let limbs: i32 = 4})
let b256_from_string = from_string b256.core

module b8: field = big_field (make_field u8 { let limbs: i32 = 1})
let b8_from_string = from_string b8.core

module b16: field = big_field (make_field u8 { let limbs: i32 = 2})
let b16_from_string = from_string b16.core


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

--let n = b32.from_string("123")
