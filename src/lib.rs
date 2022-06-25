//! This crate is a tiny utility library for finding the intersection two 2D line segments, rays,
//! or complete lines. You'll need the `geo` crate to use this crate, because this library uses
//! `geo`'s data structures.
//!
//! To use this library, construct a `LineSegment<T: num_traits::Float>` and `relate` it with
//! another `LineSegment`. This will return a `LineRelation<T>`, which covers all the possible
//! relations between those two lines.
//!
//! `LineRelation<T>` provides `unique_intersection`, if all you care about is finding a unique
//! intersection, and (for example) you don't care to distinguish cases where there are zero or an
//! infinite number of intersections.
//!
//! Here's an example of usage:
//!
//! ```
//! extern crate geo;
//! extern crate line_intersection;
//!
//! fn main() {
//!     // find the intersection of a line segment and an infinite line
//!     use line_intersection::{LineInterval, LineRelation};
//!     use geo::{Coordinate, Line, Point};
//!
//!     let segment = LineInterval::line_segment(Line {
//!         start: (0.0, 0.0).into(),
//!         end: (3.0, 3.0).into(),
//!     });
//!
//!     let line = LineInterval::line(Line {
//!         start: (2.0, 0.0).into(),
//!         end: (2.0, 0.1).into(),
//!     });
//!
//!     let intersection = segment.relate(&line).unique_intersection();
//!     assert_eq!(Some(Point(Coordinate { x: 2.0, y: 2.0 })), intersection);
//! }
//! ```

extern crate geo;

use geo::{GeoFloat, Line, Point};

/// An interval (continuous subset) of a line.
///
/// `interval_of_intersection` represents what subset of a line this `LineInterval` represents. If
/// `interval_of_intersection` is `[-Infinity, Infinity]`, then it's a line going through
/// `line.start` and `line.end`; if it's `[0, Infinity]` it's a ray, starting at `line.start`
/// extending infinitely in the direction of `line.end` and beyond; if it's `[0, 1]`, it's a line
/// segment from `line.from` to `line.end`.
///
/// It should always be the case that `interval_of_intersection.0 < interval_of_intersection.1`,
/// unless you want a degenerate line that cannot be intersected.
#[derive(Debug, PartialEq)]
pub struct LineInterval<T: GeoFloat> {
    pub line: Line<T>,
    pub interval_of_intersection: (T, T),
}

/// The relationship between two line segments.
#[derive(Debug, PartialEq)]
pub enum LineRelation<T: GeoFloat> {
    /// The line intervals are not parallel (or anti-parallel), and "meet" each other at exactly
    /// one point.
    DivergentIntersecting(Point<T>),
    /// The line intervals are not parallel (or anti-parallel), and do not intersect; they "miss"
    /// each other.
    DivergentDisjoint,
    /// The line intervals lie on the same line. They may or may not overlap, and this intersection
    /// is possibly infinite.
    Collinear,
    /// The line intervals are parallel or anti-parallel.
    Parallel,
}

impl<T: GeoFloat> LineRelation<T> {
    pub fn unique_intersection(self) -> Option<Point<T>> {
        match self {
            LineRelation::DivergentIntersecting(p) => Some(p),
            _ => None,
        }
    }
}

impl<T: GeoFloat> LineInterval<T> {
    pub fn line_segment(line: Line<T>) -> LineInterval<T> {
        LineInterval {
            line: line,
            interval_of_intersection: (T::zero(), T::one()),
        }
    }

    pub fn ray(line: Line<T>) -> LineInterval<T> {
        LineInterval {
            line: line,
            interval_of_intersection: (T::zero(), T::infinity()),
        }
    }

    pub fn line(line: Line<T>) -> LineInterval<T> {
        LineInterval {
            line: line,
            interval_of_intersection: (T::neg_infinity(), T::infinity()),
        }
    }

    /// Get the relationship between this line segment and another.
    pub fn relate(&self, other: &LineInterval<T>) -> LineRelation<T> {
        // see https://stackoverflow.com/a/565282
        let p = self.line.start_point();
        let q = other.line.start_point();
        let r = self.line.end_point() - p;
        let s = other.line.end_point() - q;

        let r_cross_s = Self::cross(&r, &s);
        let q_minus_p = q - p;
        let q_minus_p_cross_r = Self::cross(&q_minus_p, &r);

        // are the lines are parallel?
        if r_cross_s == T::zero() {
            // are the lines collinear?
            if q_minus_p_cross_r == T::zero() {
                // the lines are collinear
                LineRelation::Collinear
            } else {
                // the lines are parallel but not collinear
                LineRelation::Parallel
            }
        } else {
            // the lines are not parallel
            let t = Self::cross(&q_minus_p, &Self::div(&s, r_cross_s));
            let u = Self::cross(&q_minus_p, &Self::div(&r, r_cross_s));

            // are the intersection coordinates both in range?
            let t_in_range =
                self.interval_of_intersection.0 <= t && t <= self.interval_of_intersection.1;
            let u_in_range =
                other.interval_of_intersection.0 <= u && u <= other.interval_of_intersection.1;

            if t_in_range && u_in_range {
                // there is an intersection
                LineRelation::DivergentIntersecting(Self::t_coord_to_point(&p, &r, t))
            } else {
                // there is no intersection
                LineRelation::DivergentDisjoint
            }
        }
    }

    fn cross(a: &Point<T>, b: &Point<T>) -> T {
        a.x() * b.y() - a.y() * b.x()
    }

    fn div(a: &Point<T>, b: T) -> Point<T> {
        (a.x() / b, a.y() / b).into()
    }

    fn t_coord_to_point(p: &Point<T>, r: &Point<T>, t: T) -> Point<T> {
        (p.x() + t * r.x(), p.y() + t * r.y()).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn divergent_intersecting_segments() {
        let a = Line {
            start: (1.0, 0.0).into(),
            end: (1.0, 1.0).into(),
        };
        let b = Line {
            start: (0.0, 0.0).into(),
            end: (2.0, 0.5).into(),
        };
        let s1 = LineInterval::line_segment(a);
        let s2 = LineInterval::line_segment(b);
        let relation = LineRelation::DivergentIntersecting((1.0, 0.25).into());

        assert_eq!(relation, s1.relate(&s2));
        assert_eq!(relation, s2.relate(&s1));
    }

    #[test]
    fn divergent_intersecting_segment_and_ray() {
        let a = Line {
            start: (0.0, 0.0).into(),
            end: (1.0, 1.0).into(),
        };
        let b = Line {
            start: (2.0, 0.0).into(),
            end: (2.0, 3.0).into(),
        };
        let s1 = LineInterval::ray(a);
        let s2 = LineInterval::line_segment(b);
        let relation = LineRelation::DivergentIntersecting((2.0, 2.0).into());

        assert_eq!(relation, s1.relate(&s2));
        assert_eq!(relation, s2.relate(&s1));
    }

    #[test]
    fn divergent_disjoint_segments() {
        let a = Line {
            start: (0.0, 0.0).into(),
            end: (1.0, 1.0).into(),
        };
        let b = Line {
            start: (3.0, 0.0).into(),
            end: (0.0, 3.0).into(),
        };
        let s1 = LineInterval::line_segment(a);
        let s2 = LineInterval::line_segment(b);
        let relation = LineRelation::DivergentDisjoint;

        assert_eq!(relation, s1.relate(&s2));
        assert_eq!(relation, s2.relate(&s1));
    }

    #[test]
    fn divergent_disjoint_ray_and_line() {
        let a = Line {
            start: (1.0, 1.0).into(),
            end: (0.0, 0.0).into(),
        };
        let b = Line {
            start: (3.0, 0.0).into(),
            end: (0.0, 3.0).into(),
        };
        let s1 = LineInterval::ray(a);
        let s2 = LineInterval::line(b);
        let relation = LineRelation::DivergentDisjoint;

        assert_eq!(relation, s1.relate(&s2));
        assert_eq!(relation, s2.relate(&s1));
    }

    #[test]
    fn parallel_disjoint_segments() {
        let a = Line {
            start: (0.0, 0.0).into(),
            end: (1.0, 1.0).into(),
        };
        let b = Line {
            start: (0.0, 1.0).into(),
            end: (1.0, 2.0).into(),
        };
        let s1 = LineInterval::line(a);
        let s2 = LineInterval::line(b);
        let relation = LineRelation::Parallel;

        assert_eq!(relation, s1.relate(&s2));
        assert_eq!(relation, s2.relate(&s1));
    }

    #[test]
    fn collinear_overlapping_segment_and_line() {
        let a = Line {
            start: (0.0, 0.0).into(),
            end: (0.0, 1.5).into(),
        };
        let b = Line {
            start: (0.0, 4.0).into(),
            end: (0.0, 5.0).into(),
        };
        let s1 = LineInterval::line(a);
        let s2 = LineInterval::ray(b);
        let relation = LineRelation::Collinear;

        assert_eq!(relation, s1.relate(&s2));
        assert_eq!(relation, s2.relate(&s1));
    }
}
