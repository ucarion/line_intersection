//! This crate is a tiny utility library for finding the intersection of two 2D line segments.
//!
//! To use this library, construct a `LineSegment<T: num_traits::Float>` and `relate` it with
//! another `LineSegment`. This will return a `LineRelation<T>`, which covers all the possible
//! relations between those two lines.
//!
//! `LineRelation<T>` provides `unique_intersection`, if all you care about is finding a unique
//! intersection, and (for example) you don't care to distinguish cases where there are zero or an
//! infinite number of intersections.
//!
//! For your convenience, this library provides a top-level `relate` function for comparing two
//! values that can be converted into `LineSegment`s. Such a conversion has been provided for `[[T;
//! 2]; 2]`.
//!
//! ```
//! use line_segment_intersection::relate;
//!
//! let line1 = [[0.0, 0.0], [1.0, 1.0]];
//! let line2 = [[1.0, 0.0], [0.0, 1.0]];
//!
//! assert_eq!(Some([0.5, 0.5]), relate(&line1, &line2).unique_intersection());
//! ```

extern crate num_traits;

use num_traits::Float;

/// A line segment in two-dimensional space, defined from its two endpoints.
///
/// For the purposes of this library, the order of `from` and `to` does not matter; swapping the
/// values of `from` and `to` will never affect the result of calling `LineSegment::relate`.
pub struct LineSegment<T> {
    pub from: [T; 2],
    pub to: [T; 2],
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

impl<T> LineRelation<T> {
    /// Get the point of intersection if this is a `DivergentIntersecting`, else return `None`.
    pub fn unique_intersection(self) -> Option<[T; 2]> {
        match self {
            LineRelation::DivergentIntersecting(p) => Some(p),
            _ => None,
        }
    }
}

impl<T: Float> LineSegment<T> {
    /// Get the relationship between this line segment and another.
    pub fn relate(&self, other: &LineSegment<T>) -> LineRelation<T> {
        let sub = |a: &[T; 2], b: &[T; 2]| -> [T; 2] {
            [a[0] - b[0], a[1] - b[1]]
        };

        let cross = |a: &[T; 2], b: &[T; 2]| -> T {
            a[0] * b[1] - a[1] * b[0]
        };

        let dot = |a: &[T; 2], b: &[T; 2]| -> T {
            a[0] * b[0] + a[1] * b[1]
        };

        let div = |a: &[T; 2], b: T| -> [T; 2] {
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
        if r_cross_s == T::zero() {
            // are the lines collinear?
            if q_minus_p_cross_r == T::zero() {
                // the lines are collinear, so get coordinates of `other` along `self` and see if
                // they overlap with [0, 1] (which are the coordinates of `self` along `self`).
                let r_norm = div(&r, dot(&r, &r));

                let t0 = dot(&q_minus_p, &r_norm);
                let t1 = t0 + dot(&s, &r_norm);

                // do the ranges overlap with [0, 1]?
                if t0.min(t1) <= T::one() && t0.max(t1) >= T::zero() {
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
            if T::zero() <= t && t <= T::one() && T::zero() <= u && u <= T::one() {
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

/// Convert `a` and `b` into `LineSegment`s, and `relate` them.
pub fn relate<T: Float, S: Into<LineSegment<T>>>(a: S, b: S) -> LineRelation<T> {
    a.into().relate(&b.into())
}

impl<'a, T: Copy> From<&'a [[T; 2]; 2]> for LineSegment<T> {
    /// Construct a `LineSegment` from a pair of points; the first `[T; 2]` will be `from`, and the
    /// second will be `to`.
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
        assert_eq!(Some([1.0, 0.25]), relate(&a, &b).unique_intersection());
    }

    #[test]
    fn divergent_disjoint() {
        let a = [[0.0, 0.0], [1.0, 1.0]];
        let b = [[3.0, 0.0], [0.0, 3.0]];
        test_point_relations(&a, &b, LineRelation::DivergentDisjoint);
        assert_eq!(None, relate(&a, &b).unique_intersection());
    }

    #[test]
    fn parallel_disjoint() {
        let a = [[0.0, 0.0], [1.0, 1.0]];
        let b = [[0.0, 1.0], [1.0, 2.0]];
        test_point_relations(&a, &b, LineRelation::ParallelDisjoint);
        assert_eq!(None, relate(&a, &b).unique_intersection());
    }

    #[test]
    fn collinear_overlapping() {
        let a = [[0.0, 0.0], [0.0, 2.0]];
        let b = [[0.0, 1.0], [0.0, 1.5]];
        test_point_relations(&a, &b, LineRelation::CollinearOverlapping);
        assert_eq!(None, relate(&a, &b).unique_intersection());
    }

    #[test]
    fn collinear_disjoint() {
        let a = [[0.0, 0.0], [0.0, 1.0]];
        let b = [[0.0, 2.0], [0.0, 3.0]];
        test_point_relations(&a, &b, LineRelation::CollinearDisjoint);
        assert_eq!(None, relate(&a, &b).unique_intersection());
    }

    fn test_point_relations(a: &[[f64; 2]; 2], b: &[[f64; 2]; 2], relation: LineRelation<f64>) {
        assert_eq!(relation, relate(a, b));
        assert_eq!(relation, relate(b, a));
    }

}
