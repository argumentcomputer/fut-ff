let P_STRING_SIZE: i32 = 100 -- This is a horrible hack, but it's the only way I can find to get this to work with size types.

let pstr (s: *[]u8): [P_STRING_SIZE]u8 =
  let l = length s in
  map (\i -> if i < l then s[i] else ' ') (iota P_STRING_SIZE)

module type field_params = {
  val limbs : i32  -- Number of limbs
  val p: [P_STRING_SIZE]u8 -- Size of prime field
}

module type fieldtype = {
  type t

  val limbs: i32 -- number of limbs
  val double_limbs: i32 -- 2 x number of limbs
  val num_bits: i32
  val zero: t
  val one: t
  val highest: t
  val p_str: [P_STRING_SIZE]u8

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
  let p_str = P.p-- :> [p_n]u8

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
  type es -- element string
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

  val mac_with_carry: s -> s -> s -> s -> (s, s)
  val mac_with_carry8: u8 -> u8 -> u8 -> u8 -> (s, s)
  val add_with_carry: s -> s -> (s, s)
  val add2_with_carry: s -> s -> s -> (s, s)

  val FIELD_P: t
  val FIELD_P_DIFF: t
  val FIELD_INV: s

  val from_string [n]: [n]u8 -> t
}

module big_field (M: fieldtype): field = {
  let DOUBLE_LIMBS = 2 * M.limbs
  let LIMBS = M.limbs

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
  let one: t = map (\x -> if x == 0 then M.one else M.zero) (iota LIMBS)

  let highest: t = map (const M.highest) (iota LIMBS)
  let fill (v: s) (count: i32) : t = map (\x -> if x < count then v else M.zero) (iota LIMBS)

  let double_highest: double_t = map (\i -> if i < LIMBS then M.highest else M.zero) (iota DOUBLE_LIMBS)

  -- Do this before redefining ==.
  let no_p_str = M.p_str == pstr("")

  let (a: t) == (b: t) : bool = and (map (uncurry M.equal) (zip a b))

  let bool_t (cond: bool): s = if cond then M.one else M.zero

  let (a: t) >= (b: t) : bool =
    let res = loop (acc, i) = (true, LIMBS - 1) while acc && (i >= 0) do
                if M.(a[i] >= b[i]) then (true, i - 1) else (false, 0) in
    res.0

  let (a: t) > (b: t) : bool =
      let res = loop (acc, i) = (true, LIMBS - 1) while acc && (i i32.>= 0) do
                if M.(a[i] > b[i]) then (true, i - 1) else (false, 0) in
    res.0

  let (a: t) <= (b: t) : bool = b > a

  let (a: t) < (b: t) : bool = b >= a

  let add (a: t) (b: t): t =
    let r: *t = copy zero in
    let (_, r) = loop (carry, r) = (M.zero, r) for i < LIMBS do
      let old = a[i] in
      let tmp = M.(a[i] + b[i] + carry) in
      let carry = if M.(carry > zero) then
                    bool_t M.(old > tmp)
                  else
                    bool_t M.(old > tmp) in
      (carry, r with [i] = tmp) in
    r

--  let p_in_range = assert ((add FIELD_P FIELD_P) < FIELD_P) true

  let sub (a: t) (b: t) : t =
    let r: *t = copy zero in
    let (_, r) = loop (borrow, (r: *t)) = (M.zero, r) for i < LIMBS do
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
      let borrow = if M.(borrow > zero) then
                     bool_t M.(old <= tmp)
                   else
                     bool_t M.(old < tmp) in
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
      let res = loop (acc, i) = (true, LIMBS - 1) while acc && (i i32.>= 0) do
                  if M.(a[i] < f[i]) then (true, i - 1) else (false, 0) in
      res.0


  -- So we can read the initial FIELD_P value from a string.
  let naive_reduce (to_reduce: double_t): t =
    let in_field = loop to_reduce = to_reduce while !(double_in_field to_reduce zero) do
      double_sub to_reduce (copy double_highest) in
    map (\i -> in_field[i]) (iota LIMBS)

  let big_mul (a: []M.t) (b: t): []M.t =
    let (res: *double_t) = map (const M.zero) (iota DOUBLE_LIMBS) in
    let (outer, _) =
      loop (outer, i) = (res, 0) while i i32.< LIMBS do
      let (inner, carry) =
        (loop (inner, carry) = (outer, M.zero) for j < LIMBS do
         let box = inner[i + j] in
         let (sum, carry) = mac_with_carry a[i] b[j] (copy box) carry in
         (inner with [i + j] = sum, carry)) in
      (inner with [i + LIMBS] = carry, i i32.+ 1) in
    outer

  let naive_mul  (a: t) (b: t) : t =
    naive_reduce (big_mul a b)

  let from_u8 (n: u8): t = fill (M.from_u8 n) 1
  let ten = (from_u8 10)

  -- FIXME: Don't expose this version. Only use for reading FIELD_P.
  -- FIXME: assert valid digits
  let from_string [n] (str: [n]u8): t =
    let parse_digit (c: u8): t = from_u8 u8.(c - '0') in
    loop acc = zero for c in str do
      if c u8.== ' ' then
        acc else
        add (naive_mul acc (copy ten)) (parse_digit c)

  -- zero means field is the size of the underlying bits.
  let FIELD_P = if no_p_str then zero else from_string M.p_str

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
--    let in_field = loop to_reduce = to_reduce while !(double_in_field to_reduce DOUBLE_FIELD_P) do
    let in_field = loop to_reduce = to_reduce while !(double_in_field to_reduce FIELD_P) do
                     double_sub to_reduce dfp in
    map (\i -> in_field[i]) (iota LIMBS)

  let simple_field_mul  (a: t) (b: t) : t =
    simple_reduce (big_mul a b)

  let mont_reduce (limbs: *double_t): t =
    let (FIELD_P, FIELD_INV) = (copy (FIELD_P, FIELD_INV)) in
    let carry2 = M.zero in
    let (outer, _) = loop (outer, carry2) = (limbs, carry2) for i < LIMBS do
               let u = M.(FIELD_INV * outer[i]) in
               let carry = M.zero in
               let (inner1, carry) = loop (inner, carry) = (outer, carry) for j < LIMBS do
                         let box = inner[i + j] in
                         let (x, carry) = mac_with_carry u FIELD_P[j] (copy box) carry in
                         (inner with [i + j] = x, carry) in
               let box = [inner1[i + LIMBS]] in
               let (x, carry2) = add2_with_carry box[0] carry carry2 in
               (inner1 with [i + LIMBS] = x, carry2)  in
    let result: *t = map (\i -> outer[i + LIMBS]) (iota LIMBS) in
    if result >= FIELD_P then
      sub result FIELD_P
    else result

  let final_reduce (v: t): t =
    let double = map (\i -> if i i32.< LIMBS then v[i] else M.zero) (iota DOUBLE_LIMBS) in
    mont_reduce double

  let R_MOD_P = let z: *t = (copy zero) in sub z FIELD_P
  let to_mont(v: t): t =
    simple_reduce (big_mul (copy R_MOD_P) v)

  let mont_field_mul  (a: t) (b: t) : t =
    mont_reduce (big_mul a b)


  let (a: t) * (b: t) : t =
    simple_field_mul a b

  let (a: t) - (b: t): t =
    let old = copy a in
    let res = sub a b in
    if old >= b then res else add res (copy FIELD_P)

  let (a: t) + (b: t): t =
    -- Both inputs must be in field.
    let fzero = FIELD_P == zero in
    -- (Can move or remove this check eventually, as long as the invariant is otherwise enforced.)
    let _ = if fzero then () else assert ((a < FIELD_P) && (b < FIELD_P)) () in
    let res = add a b in
    if fzero then res else -- special case for simple bignum (make own module? or at least abstract this check)
    let fp = (copy FIELD_P) in
    let fpd = (copy FIELD_P_DIFF) in
    if res >= a then -- Can we skip this check and instead check the carry? (would need to return it)
    (if res >= fp then
       sub res fp else res) else
      add res fpd

  let square (x: t): t =
    let (res: *double_t) = map (const M.zero) (iota DOUBLE_LIMBS) in
    let res =
      (loop (outer) = res for i < LIMBS do
      let (inner, carry, _) =
        (loop (inner, carry, j) = (outer, M.zero, i i32.+ 1) while j i32.< LIMBS do
         let box = inner[i i32.+ j] in
         let (sum, carry) = mac_with_carry x[i] x[j] (copy box) carry in
         (inner with [i i32.+ j] = sum, carry, j i32.+ 1)) in
      inner with [i i32.+ LIMBS] = carry) in
    let box = res[i32.(LIMBS * 2 - 2)] in
    let res = res with [i32.(LIMBS * 2 - 1)] = (copy box) M.>> (i32.(LIMBS - 1)) in

    let (res, _) = loop (res, i) = (res, i32.(LIMBS * 2 - 2)) while (i i32.> 1) do
                   let box = res[i] in
                   let box2 = res[i i32.- 1] in
                   let res = res with [i] = (copy box) M.<< 1 M.| ((copy box2) M.>> 1) in
                   (res, i i32.+ 1) in
    let (_, _, res) = loop (i, carry, res) = (0, M.zero, res) while i i32.< LIMBS do
              let box = res[i i32.* 2] in
              let (x, carry) = mac_with_carry x[i] x[i] (copy box) carry in
              let box2 = res[i32.(i * 2 + 1)] in
              let (y, carry) = add_with_carry (copy box2) carry in
              let res = res with [i32.(i * 2)] = x in
              let res = res with [i32.(i * 2 + 1)] = y in
              (i i32.+ 1, carry, res) in
    simple_reduce res

  let double (_x: t): t = assert false (copy zero) -- TODO: implement
  let pow (_base: t) (_exp: i32): t = assert false (copy zero) -- TODO: implement
  let pow_lookup (_base: t) (_exp: i32): t = assert false (copy zero) -- FIXME: implement

  let mont_u8 (n: u8): t = to_mont (from_u8 n)
  let s_from_u8 (n: u8): s = M.from_u8 n
}

module b32: field = big_field (make_field u8 { let limbs = 4i32
                                               let p = pstr("")})
module b64: field = big_field (make_field u16 { let limbs = 4i32
                                               let p = pstr("")})
module b24: field = big_field (make_field u8 { let limbs = 3i32
                                              let p = pstr("")})
module b256: field = big_field (make_field u64 { let limbs = 4i32
                                                let p = pstr("")})
module b8: field = big_field (make_field u8 { let limbs = 1i32
                                             let p = pstr("")})
module b8': field = big_field (make_field u8 { let limbs = 1i32
                                              let p = pstr("251")})
module b16: field = big_field (make_field u8 { let limbs = 2i32
                                               let p = pstr("")})
