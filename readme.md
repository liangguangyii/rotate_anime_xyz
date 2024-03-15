## `rotate_a2b` 

Rotate vector `a` to the direction of vector `b` in 3D space.

## Usage

In the AIMD case, which means that the `pos.xyz` generated from orca has been already centerlize on COM, the rotatation will not change the origin, then there's no need to centerlize COM once again.

However for other cases, it should be taken into account.