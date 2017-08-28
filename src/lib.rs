extern crate num_traits;

use num_traits::Float;

pub struct LineSegment<T> {
    from: [T; 2],
    to: [T; 2],
}

/// The relationship between two line segments.
#[derive(Debug, PartialEq)]
pub enum LineRelation<T> {
    /// The two lines are not parallel (nor anti-parllel), and intersect at exactly one point.
    DivergentIntersecting([T; 2]),

    /// The two lines are not parallel (nor anti-parallel), and do not intersect; they "miss" each
    /// other.
    DivergentDisjoint,

    /// The two lines are collinear, and overlap one another; there may be an infinite number of
    /// intersections between them.
    ///
    /// *Note:* The case of two collinear line segments sharing one endpoint is covered by this
    /// case. An example of this is the line segments `[0, 0] -> [0, 1]` and `[0, 1] -> [0, 2]`.
    /// Aside from this special case, overlapping collinear lines have an infinite number of
    /// intersections.
    CollinearOverlapping,

    /// The two lines are collinear, but do not overlap one another.
    CollinearDisjoint,

    /// The two lines are parallel (or anti-parallel) and not collinear.
    ParallelDisjoint,
}

impl LineSegment<f64> {
    pub fn relate(&self, other: &LineSegment<f64>) -> LineRelation<f64> {
        let sub = |a: &[f64; 2], b: &[f64; 2]| -> [f64; 2] {
            [a[0] - b[0], a[1] - b[1]]
        };

        let cross = |a: &[f64; 2], b: &[f64; 2]| -> f64 {
            a[0] * b[1] - a[1] * b[0]
        };

        let dot = |a: &[f64; 2], b: &[f64; 2]| -> f64 {
            a[0] * b[0] + a[1] * b[1]
        };

        let div = |a: &[f64; 2], b: f64| -> [f64; 2] {
            [a[0] / b, a[1] / b]
        };

        // see https://stackoverflow.com/a/565282
        let p = self.from;
        let q = other.from;
        let r = sub(&self.to, &self.from);
        let s = sub(&other.to, &other.from);

        let r_cross_s = cross(&r, &s);
        let q_minus_p = sub(&q, &p);
        let q_minus_p_cross_r = cross(&q_minus_p, &r);

        // are the lines are parallel?
        if r_cross_s == 0.0 {
            // are the lines collinear?
            if q_minus_p_cross_r == 0.0 {
                // the lines are collinear, so get coordinates of `other` along `self` and see if
                // they overlap with [0, 1] (which are the coordinates of `self` along `self`).
                let r_norm = div(&r, dot(&r, &r));

                let t0 = dot(&q_minus_p, &r_norm);
                let t1 = t0 + dot(&s, &r_norm);

                // do the ranges overlap with [0, 1]?
                if t0.min(t1) <= 1.0 && t0.max(t1) >= 0.0 {
                    LineRelation::CollinearOverlapping
                } else {
                    LineRelation::CollinearDisjoint
                }
            } else {
                // the lines are parallel but not collinear
                LineRelation::ParallelDisjoint
            }
        } else {
            // the lines are not parallel
            let t = cross(&q_minus_p, &div(&s, r_cross_s));
            let u = cross(&q_minus_p, &div(&r, r_cross_s));

            // are the intersection coordinates both in range?
            if 0.0 <= t && t <= 1.0 && 0.0 <= u && u <= 1.0 {
                // there is an intersection
                let intersection_point = [p[0] + t * r[0], p[1] + t * r[1]];
                LineRelation::DivergentIntersecting(intersection_point)
            } else {
                // there is no intersection
                LineRelation::DivergentDisjoint
            }
        }
   }
}

pub fn relate<S: Into<LineSegment<f64>>>(a: S, b: S) -> LineRelation<f64> {
    a.into().relate(&b.into())
}

impl<'a, T: Copy> From<&'a [[T; 2]; 2]> for LineSegment<T> {
    fn from(point_pair: &'a [[T; 2]; 2]) -> LineSegment<T> {
        LineSegment {
            from: point_pair[0],
            to: point_pair[1],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{LineRelation, relate};

    #[test]
    fn divergent_intersecting() {
        let a = [[1.0, 0.0], [1.0, 1.0]];
        let b = [[0.0, 0.0], [2.0, 0.5]];
        test_point_relations(&a, &b, LineRelation::DivergentIntersecting([1.0, 0.25]));
    }

    #[test]
    fn divergent_disjoint() {
        let a = [[0.0, 0.0], [1.0, 1.0]];
        let b = [[3.0, 0.0], [0.0, 3.0]];
        test_point_relations(&a, &b, LineRelation::DivergentDisjoint);
    }

    #[test]
    fn parallel_disjoint() {
        let a = [[0.0, 0.0], [1.0, 1.0]];
        let b = [[0.0, 1.0], [1.0, 2.0]];
        test_point_relations(&a, &b, LineRelation::ParallelDisjoint);
    }

    #[test]
    fn collinear_overlapping() {
        let a = [[0.0, 0.0], [0.0, 2.0]];
        let b = [[0.0, 1.0], [0.0, 1.5]];
        test_point_relations(&a, &b, LineRelation::CollinearOverlapping);
    }

    #[test]
    fn collinear_disjoint() {
        let a = [[0.0, 0.0], [0.0, 1.0]];
        let b = [[0.0, 2.0], [0.0, 3.0]];
        test_point_relations(&a, &b, LineRelation::CollinearDisjoint);
    }

    fn test_point_relations(a: &[[f64; 2]; 2], b: &[[f64; 2]; 2], relation: LineRelation<f64>) {
        assert_eq!(relation, relate(a, b));
        assert_eq!(relation, relate(b, a));
    }

}
