-- https://github.com/filecoin-project/pairing/blob/master/src/bls12_381/fr.rs#L9
-- #[PrimeFieldModulus = "52435875175126190479447740508185965837690552500527637822603658699938581184513"]
let bls12_381_modulus  = "52435875175126190479447740508185965837690552500527637822603658699938581184513"
let r_squared_mod_p = "3294906474794265442129797520630710739278575682199800681788903916070560242797"

module type field_params = {
  val limbs : i32  -- Number of limbs
  val p: () -> []u8 -- Size of prime field
  val r2: () -> []u8 -- R^2 mod p
}

module type fieldtype = {
  module FT: integral
  type t

  val limbs: i32 -- number of limbs
  val double_limbs: i32 -- 2 x number of limbs
  val num_bits: i32
  val zero: t
  val one: t
  val highest: t

  -- P, the prime field modulus.
  val p_str: () -> []u8

  -- R^2 mod P
  -- It would be nice to be able to calculate this, but we need it in order to make multiplication in the field cheap.
  -- For now, we will settle for verifying it once montogmery multiplication reduction is bootstrapped. (see: field_r2_is_correct)
  val r2_str: () -> []u8

  val equal: t -> t -> bool
  val gt: t -> t -> bool
  val gte: t -> t -> bool
  val lt: t -> t -> bool
  val lte: t -> t -> bool
  val add: t -> t -> t
  val mul: t -> t -> t
  val sub: t -> t -> t
  val from_u8: u8 -> t
  val from_u16: u16 -> t
  val from_u32: u32 -> t
  val from_u64: u64 -> t
  val to_u64: t -> u64
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

module fieldtype (T: integral) (P: field_params): fieldtype = {
  module FT = T
  type t = T.t

  let limbs = P.limbs
  let double_limbs = 2 * P.limbs
  let num_bits = T.num_bits
  let zero = T.highest T.- T.highest
  let one = T.highest T./ T.highest
  let highest = T.highest
  let p_str = P.p
  let r2_str = P.r2

  let equal (a: t) (b: t): bool = a T.== b
  let gt (a: t) (b: t): bool = a T.> b
  let gte (a: t) (b: t): bool = a T.>= b
  let lt (a: t) (b: t): bool = a T.< b
  let lte (a: t) (b: t): bool = a T.<= b
  let add (a: t) (b: t): t = a T.+ b
  let sub (a: t) (b: t): t = a T.- b
  let mul (a: t) (b: t): t = a T.* b
  let from_u8 (n: u8): t = T.u8 n
  let from_u16 (n: u16): t = T.u16 n
  let from_u32 (n: u32): t = T.u32 n
  let from_u64 (n: u64): t = T.u64 n
  let to_u64 (n: t): u64 = u64.i64 (T.to_i64 n)
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
  type es -- element string
  type double_t

  module S: integral -- module for limb type

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

  val big_mul: t -> t -> double_t
  val simple_reduce: double_t ->  t
--  val basic_reduce: *double_t ->  t

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

  -- These from_ functions convert from the named integral type and populate the first limb.
  -- If the value must be truncated, that will happen.
  val from_u8: u8 -> t
  val from_u16: u16 -> t
  val from_u32: u32 -> t
  val from_u64: u64 -> t

  val s_from_u8: u8 -> s
  val s_from_u16: u16 -> s
  val s_from_u32: u32 -> s
  val s_from_u64: u64 -> s

  val mac_with_carry: s -> s -> s -> s -> (s, s)
  val mac_with_carry8: u8 -> u8 -> u8 -> u8 -> (s, s)
  val add_with_carry: s -> s -> (s, s)
  val add2_with_carry: s -> s -> s -> (s, s)

  val LIMBS: i32
  val FIELD_P: t
  val FIELD_R2: t
  val FIELD_P_DIFF: t
  val FIELD_INV: s

  val from_string [n]: [n]u8 -> t
  val make: (() -> []s) -> t

  val from_u64s [n]: [n]u64 -> t
  val to_u64s: t -> [LIMBS]u64

  -- Debugging
  val double_in_field: double_t -> t -> bool
  val double_sub: double_t -> double_t -> double_t
  val DOUBLE_FIELD_P: double_t
  val R_MOD_P: t
  val double_zero: double_t
  val DOUBLE_R: double_t

}

module big_field (M: fieldtype): field = {
  let DOUBLE_LIMBS = 2 * M.limbs
  let LIMBS = M.limbs
  module S = M.FT

  type t = [LIMBS]M.t
  type s = M.t
  type double_t = [DOUBLE_LIMBS]s

  let digit_count: i32 =
    let digits_per_byte = f64.log10(2 ** 8) in
    let bytes = LIMBS * (M.num_bits / 8) in
    i32.f64 (f64.ceil ((f64.i32 bytes) * digits_per_byte))

  type es = [digit_count]u8

  let limbs_are_unsigned = assert (M.lt M.zero (M.sub M.zero M.one)) true

  let zero: t = map (\_ -> M.zero) (iota LIMBS)
  let double_zero: double_t = map (\_ -> M.zero) (iota DOUBLE_LIMBS)
  let DOUBLE_R: double_t = map (\i -> if i == LIMBS then M.one else M.zero) (iota DOUBLE_LIMBS)
  let one: t = map (\x -> if x == 0 then M.one else M.zero) (iota LIMBS)

  let highest: t = map (const M.highest) (iota LIMBS)
  let fill (v: s) (count: i32) : t = map (\x -> if x < count then v else M.zero) (iota LIMBS)

  let double_highest: double_t = map (\i -> if i < LIMBS then M.highest else M.zero) (iota DOUBLE_LIMBS)

  -- Do this before redefining ==.
  let no_p_str = length (M.p_str ()) == 0

  -- Primality is unchecked, but distinguish between the bignum and prime field cases. (TODO: separate the modules).
  let is_prime_field = assert (!no_p_str) true

  let (a: t) == (b: t) : bool = and (map (uncurry M.equal) (zip a b))

  let bool_t (cond: bool): s = if cond then M.one else M.zero

  let (a: t) >= (b: t) : bool =
    let res = loop (acc, i) = (true, LIMBS - 1) while acc && (i >= 0) do
                if M.(a[i] > b[i]) then (true, -1) else
                  (M.(a[i] == b[i]), i-1) in
    res.0

  let (a: t) > (b: t) : bool =
      let res = loop (acc, i) = (true, LIMBS - 1) while acc && (i i32.>= 0) do
                  if M.(a[i] == b[i]) then (true, i - 1) else
                    (M.(a[i] > b[i]), -1) in
    res.0

  let (a: t) <= (b: t) : bool = b > a

  let (a: t) < (b: t) : bool = b >= a

  let add (a: t) (b: t): t =
    let r: *t = copy zero in
    let carry = M.zero in
    let (_, r) = loop (carry, r) for i < LIMBS do
      let old = a[i] in
      let tmp = M.(a[i] + b[i] + carry) in
      let carry = if M.(carry > zero) then
                    bool_t M.(old > tmp)
                  else
                    bool_t M.(old > tmp) in
      (carry, r with [i] = tmp) in
    r

  let sub (a: t) (b: t) : t =
    let r: *t = copy zero in
    let borrow = M.zero in
    let (_, r) = loop (borrow, (r: *t)) for i < LIMBS do
      let old = a[i] in
      let tmp = M.(a[i] - (b[i] + borrow)) in
      let borrow = if M.(borrow > zero) then
                     bool_t M.(old <= tmp)
                   else
                     bool_t M.(old < tmp) in
      (borrow, r with [i] = tmp) in
    r

  --  TODO: try to make this generic over size.
  let double_sub (a: double_t) (b: double_t) : double_t =
    let r: *double_t = copy double_zero in
    let (_, r) = loop (borrow, r) = (M.zero, r) for i < DOUBLE_LIMBS do
      let old = a[i] in
      let tmp = M.(a[i] - (b[i] + borrow)) in
      let borrow = bool_t (if M.(borrow > zero) then M.(old <= tmp) else M.(old < tmp)) in
      (borrow, r with [i] = tmp) in
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

    -- Is double-width a in the field?
    let double_in_field (a: double_t) (f: t): bool =
      all (\i -> M.(a[i i32.+ limbs] == zero)) (iota LIMBS) &&
      if f == zero then true else
      let (acc, _) = loop (acc, i) = (true, LIMBS - 1) while acc && (i i32.>= 0) do
                  if M.(a[i] < f[i]) then (true, -1) else
                    (M.(a[i] == f[i]), i - 1) in
      acc

  -- So we can read the initial FIELD_P value from a string.
  -- Naive means does not respect modulus.
  let naive_reduce (to_reduce: double_t): t =
    let in_field = loop to_reduce while !(double_in_field to_reduce zero) do
      double_sub to_reduce (copy double_highest) in
    map (\i -> in_field[i]) (iota LIMBS)

  let big_mul (a: t) (b: t): [DOUBLE_LIMBS]M.t =
    let (res: *double_t) = map (const M.zero) (iota DOUBLE_LIMBS) in
    let (outer, _) =
      loop (outer, i) = (res, 0) while i i32.< LIMBS do
      let (inner, carry) =
        (loop (inner, carry) = (outer, M.zero) for j < LIMBS do
         let box = inner[i + j] in
         let (sum, carry) = mac_with_carry a[i] b[j] (copy box) carry in
         (inner with [i + j] = sum, carry)) in
      (inner with [i + LIMBS] = carry, i + 1) in
    outer

  let naive_mul  (a: t) (b: t) : t =
    naive_reduce (big_mul a b)

  let from_u8 (n: u8): t = fill (M.from_u8 n) 1
  let from_u16 (n: u16): t = fill (M.from_u16 n) 1
  let from_u32 (n: u32): t = fill (M.from_u32 n) 1
  let from_u64 (n: u64): t = fill (M.from_u64 n) 1
  let ten = (from_u8 10)

  -- FIXME: Don't expose this version. Only use for reading FIELD_P.
  -- FIXME: assert valid digits
  let from_string [n] (str: [n]u8): t =
    let parse_digit (c: u8): t = from_u8 u8.(c - '0') in
    loop acc = copy zero for c in str do
      if c u8.== ' ' then
        acc else
        add (naive_mul acc (copy ten)) (parse_digit c)

  -- zero means field is the size of the underlying bits.
  let FIELD_P = if no_p_str then zero else from_string (M.p_str ())
  let FIELD_R2 = if no_p_str then zero else from_string (M.r2_str ())

  -- Could also do this by checking that the most-significant bit is 0.
  -- Is p less than half of the field size?
  let p_is_small = (add FIELD_P FIELD_P) >= FIELD_P

  let DOUBLE_FIELD_P: double_t =
    let fp = copy FIELD_P in
    map (\i -> if i i32.< LIMBS then fp[i] else M.zero) (iota DOUBLE_LIMBS)

  let FIELD_P_DIFF = sub zero FIELD_P

  let calc_inv (a: s): s =
    let inv = (loop inv = M.one for _i < M.num_bits do
               let inv = M.(inv * inv) in M.(inv * a))
    in M.(zero - inv)

  let FIELD_INV = calc_inv FIELD_P[0]

  let simple_reduce (to_reduce: double_t): t =
    let dfp = (copy DOUBLE_FIELD_P) in
    let in_field = loop to_reduce while !(double_in_field to_reduce FIELD_P) do
         double_sub to_reduce dfp in
    map (\i -> in_field[i]) (iota LIMBS)

  let simple_field_mul  (a: t) (b: t) : t =
    simple_reduce (big_mul a b)

  let mont_reduce (limbs: *double_t): t =
    let (FIELD_P, FIELD_INV) = (copy (FIELD_P, FIELD_INV)) in
    let (lmbs, _) =
      loop (lmbs, carry2) = (limbs, M.zero) for i < LIMBS do
      let u = M.(FIELD_INV * lmbs[i]) in
      let carry = M.zero in
      let (lmbs, carry) =
        loop (lmbs, carry) for j < LIMBS do
        let box = lmbs[i + j] in
        let (x, carry) = mac_with_carry u FIELD_P[j] (copy box) carry in
        (lmbs with [i + j] = x, carry) in
      let box = [lmbs[i + LIMBS]] in
      let (x, carry2) = add2_with_carry box[0] carry carry2 in
      (lmbs with [i + LIMBS] = x, carry2)  in
    let result: t = map (\i -> lmbs[i + LIMBS]) (iota LIMBS) in
    if result >= FIELD_P then
      sub result FIELD_P
    else result

  let final_reduce (v: t): t =
    let double = map (\i -> if i i32.< LIMBS then v[i] else M.zero) (iota DOUBLE_LIMBS) in
    mont_reduce double

  let R_MOD_P = if FIELD_P == zero then zero else simple_reduce DOUBLE_R

  let to_mont(v: t): t =
    mont_reduce (big_mul (copy FIELD_R2) v)

  let mont_field_mul  (a: t) (b: t) : t =
    mont_reduce (big_mul a b)


  let (a: t) * (b: t) : t =
    mont_field_mul a b

  -- Be paranoid and fail if the provided value for r^2 mod p is incorrect.
  let field_r2_is_correct = assert (final_reduce FIELD_R2 == R_MOD_P) true

  let (a: t) - (b: t): t =
    let old = copy a in
    let res = sub a b in
    if old >= b then res else add res (copy FIELD_P)

  let add_expensive (a: t) (b: t): t =
    -- Both inputs must be in field.
    let fzero = FIELD_P == zero in
    -- (Can move or remove this check eventually, as long as the invariant is otherwise enforced.)
    let _ = if fzero then () else assert ((a < FIELD_P) && (b < FIELD_P)) () in
    let res = add a b in
    if fzero then res else -- special case for simple bignum (make own module? or at least abstract this check)
    if res >= a then -- Can we skip this check and instead check the carry? (would need to return it)
    (if res >= FIELD_P then
       sub res (copy FIELD_P) else res) else
      add res (copy FIELD_P_DIFF)

  let add_cheap (a: t) (b: t): t =
    -- Both inputs must be in field.
    let fzero = FIELD_P == zero in
    -- (Can move or remove this check eventually, as long as the invariant is otherwise enforced.)
    let _ = if fzero then () else assert ((a < FIELD_P) && (b < FIELD_P)) () in
    let res = add a b in
    if res >= FIELD_P then sub res (copy FIELD_P) else res

  let (a: t) + (b: t) = if p_is_small then add_cheap a b else add_expensive a b

  let square (x: t): t =
    let _ = assert false "unimplemented" in
    -- This implementation is failing with large enough values, example test case:
    -- bls12_381.((from_string "12345678912345678912345678901") * (from_string "12345678912345678912345678901") == (square (from_string "12345678912345678912345678901")))
    -- should return true, but returns false.

    -- Long multiplication (Diagonal elements are skipped)
    let (res: *double_t) = replicate DOUBLE_LIMBS M.zero in
    let res =
      loop res for i < i32.(LIMBS-1) do
      let (res, carry, _) =
        loop (res, carry, j) = (res, M.zero, i32.(i + 1)) while i32.(j < LIMBS) do
         let (sum, carry) = mac_with_carry x[i] x[j] (copy res[i32.(i + j)]) carry
         in (res with [i32.(i + j)] = sum, carry, i32.(j + 1))
      in res with [i32.(i + LIMBS)] = carry in

    --  Double the result
    let res = res with [i32.(LIMBS * 2 - 1)] = (copy res[i32.(LIMBS * 2 - 2)]) M.>> i32.(M.num_bits - 1) in
    let (res, _) =
      loop (res, i) = (res, i32.(LIMBS * 2 - 2)) while i32.(i > 1) do
      let res = res with [i] = M.((copy res[i] << 1) | ((copy res[i32.(i - 1)] >> i32.(M.num_bits - 1)))) in
      (res, i32.(i - 1))
    in let res = res with [1] = M.(copy res[1] << 1) in

    --  Process diagonal elements
    let (_, _, res) =
      loop (i, carry, res) = (0, M.zero, res) while i32.(i < LIMBS) do
      let (a, carry) = mac_with_carry x[i] x[i] (copy res[i32.(i * 2)]) carry in
      let (b, carry) = add_with_carry (copy res[i32.(i * 2 + 1)]) carry in
      (i i32.+ 1,
       carry,
       res with [i32.(i * 2)] = a
           with [i32.(i * 2 + 1)] = b)

    in mont_reduce res

  let double (_x: t): t = assert false (copy zero) -- TODO: implement
  let pow (_base: t) (_exp: i32): t = assert false (copy zero) -- TODO: implement
  let pow_lookup (_base: t) (_exp: i32): t = assert false (copy zero) -- FIXME: implement

  let mont_u8 (n: u8): t = to_mont (from_u8 n)
  let s_from_u8 (n: u8): s = M.from_u8 n
  let s_from_u16 (n: u16): s = M.from_u16 n
  let s_from_u32 (n: u32): s = M.from_u32 n
  let s_from_u64 (n: u64): s = M.from_u64 n

  let make (a: () -> []s): t = a () :> t

  let from_u64s (limbs: []u64): t =
    assert i32.((length limbs) == LIMBS) ((map M.from_u64 limbs) :> t)

  let to_u64s (limbs: t): [LIMBS]u64 =
    map M.to_u64 limbs
}

-- Non-prime fields don't work with montgomery representation. TODO: support them separately.

-- module b32: field = big_field (fieldtype u8 { let limbs = 4i32
--                                               let p () = ""
--                                               let r2 () = "" })

-- module b64: field = big_field (fieldtype u16 { let limbs = 4i32
--                                                let p () = ""
--                                                let r2 () = "" })

-- module b24: field = big_field (fieldtype u8 { let limbs = 3i32
--                                               let p () = ""
--                                               let r2 () = "" })

-- module b256: field = big_field (fieldtype u64 { let limbs = 4i32
--                                                 let p () = ""
--                                                 let r2 () = "" })

-- module b8: field = big_field (fieldtype u8 { let limbs = 1i32
--                                              let p () = ""
--                                              let r2 () = "" })

module b8': field = big_field (fieldtype u8 { let limbs = 1i32
                                              let p () = "251"
                                              let r2 () = "25" })
-- module b16: field = big_field (fieldtype u8 { let limbs = 2i32
--                                               let p () = ""
--                                               let r2 () = "" })

module bls12_381: field = big_field (fieldtype u64 {
                                                 let limbs = 4i32
                                                 let p () = copy bls12_381_modulus
                                                 let r2 () = copy r_squared_mod_p })
