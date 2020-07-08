-- | A module type for standard Euclidean vectors.
--
-- Vectors of arbitrary dimension can be constructed with
-- `mk_vspace`@term by passing in a `vector`@mtype@"vector" and a
-- `real`@mtype@"/fublib/math" module, although for most uses the
-- `mk_vspace_2d`@term and `mk_vspace_3d`@term modules are simpler and
-- sufficient.

module type vspace = {
  type real

  -- | A vector type.  Semantically a sequence of `real`s.
  type vector

    -- | Neutral element for the addition.
  val zero: vector
  -- | Neutral element for the multiplication by a scalar.
  val one: real

  -- | Apply an operation to each element of the vector.
  val map : (real -> real) -> vector -> vector

  -- | Apply a binary operation to corresponding components of two
  -- vectors.
  val map2 : (real -> real -> real) -> vector -> vector -> vector

  -- | Add vectors elementwise.
  val +: vector -> vector -> vector
  -- | Subtract vectors elementwise.
  val -: vector -> vector -> vector
  -- | Multiply vectors elementwise.
  val *: vector -> vector -> vector
  -- | Divide vectors elementwise.
  val /: vector -> vector -> vector

  -- | Inner product.
  val dot: vector -> vector -> real

  -- | Squared norm.
  val quadrance: vector -> real

  val scale: real -> vector -> vector
  val norm: vector -> real

  -- | Transform to unit vectortor.
  val normalise: vector -> vector
}

-- | A two-dimensional vector space is just a vector space, but we
-- give the vectors a convenient record type.
module type vspace_2d = {
  type real

  include vspace with real = real with vector = {x: real, y: real}
}

-- | The operations that must be supported by the scalars contained in
-- a vector.  The builtin modules `f32`@term and `f64`@term satisfy
-- this interface.
module type scalar = {
  type t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t

  val i32: i32 -> t
  val sqrt : t -> t
}

-- | Construct a 2D vector space.
module mk_vspace_2d (real: scalar): vspace_2d with real = real.t = {
  type real = real.t

  type vector = {x: real, y: real}

  let zero = {x = real.i32 0, y = real.i32 0}
  let one  = (real.i32 1)

  let map f (v : vector) =
    {x = f v.x, y = f v.y}

  let map2 f (a : vector) (b : vector) =
    {x = f a.x b.x, y = f a.y b.y}

  let (+) = map2 (real.+)
  let (-) = map2 (real.-)
  let (*) = map2 (real.*)
  let (/) = map2 (real./)

  let dot (a: vector) (b: vector) =
    real.(a.x*b.x + a.y*b.y)

  let quadrance v = dot v v

  let scale (s: real) = map (s real.*)

  let norm = quadrance >-> real.sqrt

  let normalise (v: vector): vector =
    let l = norm v
    in scale (one real./ l) v
}

-- | A three-dimensional vector space is just a vector space, but we
-- give the vectors a convenient record type.  Also, cross product is
-- defined.
module type vspace_3d = {
  type real

  include vspace with real = real with vector = {x: real, y: real, z: real}

  -- | Cross product.
  val cross: vector -> vector -> vector

  -- | Rotate vector around the *x* axis.  This leaves the *x* axis
  -- unchanged.
  val rot_x : (radians: real) -> vector -> vector

  -- | Rotate vector around the *y* axis.  This leaves the *y* axis
  -- unchanged.
  val rot_y : (radians: real) -> vector -> vector

  -- | Rotate vector around the *z* axis.  This leaves the *z* axis
  -- unchanged.
  val rot_z : (radians: real) -> vector -> vector
}

-- | Construct a 3D vector space.
module mk_vspace_3d(real: real): vspace_3d with real = real.t = {
  type real = real.t

  type vector = {x: real, y: real, z: real}

  let zero = {x = real.i32 0, y = real.i32 0, z = real.i32 0}
  let one  = real.i32 1

  let map f (v : vector) =
    {x = f v.x, y = f v.y, z = f v.z}

  let map2 f (a : vector) (b : vector) =
    {x = f a.x b.x, y = f a.y b.y, z = f a.z b.z}

  let (+) = map2 (real.+)
  let (-) = map2 (real.-)
  let (*) = map2 (real.*)
  let (/) = map2 (real./)

  let dot (a: vector) (b: vector) =
    real.(a.x*b.x + a.y*b.y + a.z*b.z)

  let cross ({x=ax,y=ay,z=az}: vector)
            ({x=bx,y=by,z=bz}: vector): vector =
    real.({x=ay*bz-az*by, y=az*bx-ax*bz, z=ax*by-ay*bx})

  let quadrance v = dot v v

  let scale (s: real) = map (s real.*)

  let norm = quadrance >-> real.sqrt

  let normalise (v: vector): vector =
    let l = norm v
    in scale (one real./ l) v

  let rot_x (theta: real) ({x,y,z} : vector) =
    let cos_theta = real.cos theta
    let sin_theta = real.sin theta
    in { x
       , y = real.(cos_theta * y - sin_theta * z)
       , z = real.(sin_theta * y + cos_theta * z)}

  let rot_y (theta: real) ({x,y,z} : vector) =
    let cos_theta = real.cos theta
    let sin_theta = real.sin theta
    in { x = real.(cos_theta * x - sin_theta * z)
       , y
       , z = real.(sin_theta * x + cos_theta * z)}

  let rot_z (theta: real) ({x,y,z} : vector) =
    let cos_theta = real.cos theta
    let sin_theta = real.sin theta
    in { x = real.(cos_theta * x - sin_theta * y)
       , y = real.(sin_theta * x + cos_theta * y)
       , z}

}

import "vector"

-- | Construct an arbitrary-dimensional vector space.  The
-- dimensionality is given by the vector representation that is
-- provided.
module mk_vspace(V: vector) (real: real):
       vspace with real = real.t with vector = V.vector real.t = {
  type real = real.t
  type vector = V.vector real

  let zero = V.replicate (real.i32 0)
  let one: real.t  = real.i32 1

  let map = V.map
  let map2 f a b = V.zip a b |> V.map (uncurry f)

  let (+) = map2 (real.+)
  let (-) = map2 (real.-)
  let (*) = map2 (real.*)
  let (/) = map2 (real./)

  let dot (a: vector) (b: vector) =
    V.reduce (real.+) (real.i32 0) (V.map (uncurry (real.*)) (V.zip a b))

  let scale (s: real) = map (s real.*)

  let quadrance v = dot v v

  let norm = quadrance >-> real.sqrt

  let normalise (v: vector): vector =
    let l = norm v
    in scale (one real./ l) v
}
