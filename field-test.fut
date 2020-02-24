import "field"
-- Test Stringable
-- ==
-- entry: u64_from_string
-- input { [49u8, 50u8, 51u8] } output { 123u64 }
-- input { [48u8] } output { 0u64 }

-- Inputs = "123" and "0", `futhark test` doesn't parse strings, it seems.

entry u64_from_string (s: *[]u8) : u64 = u64_from_string s

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
