import "field"
-- Test bls12-381 for simple inputs whose product is trivially within the field.
-- ==
-- entry: bls12_381_test_multiplication_simple
-- input { [1u8, 9u8, 9u8, 250u8] [1u8, 23u8, 123u8, 250u8] }
-- output { [[1u64, 0u64, 0u64, 0u64], [207u64, 0u64, 0u64, 0u64], [1107u64, 0u64, 0u64, 0u64], [62500u64, 0u64, 0u64, 0u64]] }

entry bls12_381_test_multiplication_simple xs ys =
  map2 bls12_381.(\a b -> mont_to_u64s ((to_mont (from_u8 a)) * (to_mont (from_u8 b)))) xs ys

-- Check that -1 * -1 = 1.
-- ==
-- entry: bls12_381_test_xxx
-- input { } output { true }

entry bls12_381_test_xxx =
  bls12_381.(final_reduce (to_mont (zero - one)) * (to_mont (zero - one)) == one)

-- Test bls12_381 addition on demonstrated 'problem' inputs.
-- ==
-- entry: bls12_381_sum_regression
-- input { [7020848387740809376u64, 1917052142680926307u64,  16796642395158532322u64, 2102389801514013901u64]
--         [3910201068166067765u64, 16467923255106892344u64,  3800792095654130893u64, 5534858394689326649u64] }
-- output{ [10931049455906877141u64, 18384975397787818651u64, 2150690417103111599u64, 7637248196203340551u64] }

entry bls12_381_sum_regression (a: [4]u64) (b: [4]u64) =
  bls12_381.(to_u64s ((from_u64s a) + (from_u64s b)))

-- Test commutativity of +
-- ==
-- entry: bls12_381_commutative_plus
-- random input { [10000][4]u64 [10000][4]u64 }
-- output { true }

entry bls12_381_commutative_plus [n] (a: [n][4]u64) (b: [n][4]u64) =
  reduce (&&) true (map (\p ->
                           let x = bls12_381.(from_u64s p.0) in
                           let y = bls12_381.(from_u64s p.1) in
                           if bls12_381.(in_field x && in_field y)
                           then bls12_381.(x + y == y + x)
                           else true)
                        (zip a b))

-- Test associativity of +
-- ==
-- entry: bls12_381_associative_plus
-- random input { [10000][4]u64 [10000][4]u64 [10000][4]u64 }
-- output { true }

entry bls12_381_associative_plus [n] (a: [n][4]u64) (b: [n][4]u64) (c: [n][4]u64) =
  reduce (&&) true (map (\p ->
                           let x = bls12_381.(from_u64s p.0) in
                           let y = bls12_381.(from_u64s p.1.0) in
                           let z = bls12_381.(from_u64s p.1.1) in
                           if bls12_381.(in_field x && in_field y)
                           then bls12_381.((x + y) + z == x + (y + z))
                           else true)
                        (zip a (zip b c)))

-- Test that bls12_381 P is small, simplifying commutativity analysis for bls12_381.+
-- ==
-- entry: bls12_381_p_is_small
-- input {}
-- output { true }

entry bls12_381_p_is_small = bls12_381.p_is_small
