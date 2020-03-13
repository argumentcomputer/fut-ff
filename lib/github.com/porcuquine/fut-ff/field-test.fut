import "field"
-- Test bls12-381
--
-- entry: bls12_381_test_multiplication
-- input { [1, 9] [1, 23] } output { [1, 207] }

entry bls12_381_test_multiplication xs ys =
  map2 bls12_381.(\a b -> final_reduce ((to_mont (from_u8 a)) * (to_mont (from_u8 b)))) xs ys

-- entry: bls12_381_test_xxx
-- input { } output { true }

entry bls12_381_test_xxx =
  bls12_381.(final_reduce (to_mont (zero - one)) * (to_mont (zero - one)) == one)
