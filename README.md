# line\_intersection

[![Crate version shield](https://img.shields.io/crates/v/line_intersection.svg)](https://crates.io/crates/line_intersection)

[Documentation on `docs.rs`](https://docs.rs/line_intersection)

[Crate on `crates.io`](https://crates.io/crates/line_intersection)

A tiny Rust library for finding the intersection of two lines, rays, or line
segments.

```rust
extern crate geo;
extern crate line_intersection;

fn main() {
    // find the intersection of a line segment and an infinite line
    use line_intersection::{LineInterval, LineRelation};
    use geo::{Coordinate, Line, Point};

    let segment = LineInterval::line_segment(Line {
        start: (0.0, 0.0).into(),
        end: (3.0, 3.0).into(),
    });

    let line = LineInterval::line(Line {
        start: (2.0, 0.0).into(),
        end: (2.0, 0.1).into(),
    });

    let intersection = segment.relate(&line).unique_intersection();
    assert_eq!(Some(Point(Coordinate { x: 2.0, y: 2.0 })), intersection);
}
```

## License (MIT)

```
Copyright (c) 2017 Ulysse Carion

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
