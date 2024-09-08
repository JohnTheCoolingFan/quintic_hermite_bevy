use bevy::{math::VectorSpace, prelude::*};

#[derive(Debug, Clone, Copy, Component, Default)]
pub struct PolynomialCurveSegment<P: VectorSpace> {
    coeff: [P; 6],
}

impl<P: VectorSpace> PolynomialCurveSegment<P> {
    fn coefficients(p: [P; 6], char_matrix: [[f32; 6]; 6]) -> Self {
        let [c0, c1, c2, c3, c4, c5] = char_matrix;
        let coeff = [
            p[0] * c0[0] + p[1] * c0[1] + p[2] * c0[2] + p[3] * c0[3] + p[4] * c0[4] + p[5] * c0[5],
            p[0] * c1[0] + p[1] * c1[1] + p[2] * c1[2] + p[3] * c1[3] + p[4] * c1[4] + p[5] * c1[5],
            p[0] * c2[0] + p[1] * c2[1] + p[2] * c2[2] + p[3] * c2[3] + p[4] * c2[4] + p[5] * c2[5],
            p[0] * c3[0] + p[1] * c3[1] + p[2] * c3[2] + p[3] * c3[3] + p[4] * c3[4] + p[5] * c3[5],
            p[0] * c4[0] + p[1] * c4[1] + p[2] * c4[2] + p[3] * c4[3] + p[4] * c4[4] + p[5] * c4[5],
            p[0] * c5[0] + p[1] * c5[1] + p[2] * c5[2] + p[3] * c5[3] + p[4] * c5[4] + p[5] * c5[5],
        ];
        Self { coeff }
    }

    pub fn position(&self, t: f32) -> P {
        let [a, b, c, d, e, f] = self.coeff;

        a + (b + (c + (d + (e + (f) * t) * t) * t) * t) * t
    }
}

#[derive(Debug, Clone, Component)]
pub struct QuinticHermite<P: VectorSpace> {
    points: Vec<(P, P, P)>,
}

impl<P: VectorSpace> QuinticHermite<P> {
    const CHAR_MATRIX: [[f32; 6]; 6] = [
        [1.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        [0.0, 1.0, 0.0, 0.0, 0.0, 0.0],
        [0.0, 0.0, 0.5, 0.0, 0.0, 0.0],
        [-10.0, -6.0, -1.5, 10.0, -4.0, 0.5],
        [15.0, 8.0, 1.5, -15.0, 7.0, -1.0],
        [-6.0, -3.0, -0.5, 6.0, -3.0, 0.5],
    ];

    pub fn new(points: impl IntoIterator<Item = (P, P, P)>) -> Self {
        Self {
            points: Vec::from_iter(points),
        }
    }

    pub fn into_segments(self) -> Option<Vec<PolynomialCurveSegment<P>>> {
        if self.points.len() < 2 {
            None
        } else {
            Some(
                Iterator::map(
                    Iterator::map(self.points.windows(2), |pairs| {
                        let (a, b, c) = pairs[0];
                        let (d, e, f) = pairs[1];
                        [a, b, c, d, e, f]
                    }),
                    |points| PolynomialCurveSegment::coefficients(points, Self::CHAR_MATRIX),
                )
                .collect(),
            )
        }
    }
}
