use bevy::{math::VectorSpace, prelude::*};

use super::quintic_hermite::*;

impl<P: VectorSpace> PolynomialCurveSegment<P> {
    pub fn positions_iter(self, num: usize) -> impl Iterator<Item = P> {
        (0..=num).map(move |i| {
            let t = (num as f32).recip() * i as f32;
            self.position(t)
        })
    }

    pub fn segments_iter(self, num: usize) -> Vec<(P, P)> {
        let positions = self.positions_iter(num).collect::<Vec<_>>();
        positions
            .windows(2)
            .map(|seg| {
                let [start, end, ..] = *seg else {
                    unreachable!()
                };
                (start, end)
            })
            .collect()
    }
}

impl<P: VectorSpaceLength> PolynomialCurveSegment<P> {
    pub fn segment_min_max_diff(self, num: usize) -> (f32, f32, f32) {
        let mut max = f32::ZERO;
        let mut min = f32::INFINITY;
        for (start, end) in self.segments_iter(num) {
            let length = (end - start).len();
            if length > max {
                max = length
            }
            if length < min {
                min = length
            }
        }
        let diff = max - min;
        (min, max, diff)
    }
}

pub trait VectorSpaceLength: VectorSpace {
    fn len(self) -> f32;
}

impl VectorSpaceLength for Vec2 {
    fn len(self) -> f32 {
        self.length()
    }
}

impl VectorSpaceLength for Vec3 {
    fn len(self) -> f32 {
        self.length()
    }
}
