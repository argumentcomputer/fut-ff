import "field"
-- Test b32
-- ==
-- entry: b32_test_equal
-- input { } output { true }

-- Test b32_gte
-- ==
-- entry: b32_test_gte
-- input { } output { true }

open b32
entry b32_test_equal =
  one + zero == one

entry b32_test_gte =
  (one >= one) && b32.(one >= zero) && !b32.(zero >= one)
