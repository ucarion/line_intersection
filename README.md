# line\_segment\_intersection

![Crate version shield](https://img.shields.io/crates/v/line_segment_intersection.svg)

[Documentation on `docs.rs`](https://docs.rs/line_segment_intersection)

[Crate on `crates.io`](https://crates.io/crates/line_segment_intersection)

A tiny Rust library for finding the intersection of two line segments in
two-dimensional space.

```rust
extern crate line_segment_intersection;

use line_segment_intersection::relate;

let line1 = [[0.0, 0.0], [1.0, 1.0]];
let line2 = [[1.0, 0.0], [0.0, 1.0]];

assert_eq!(Some([0.5, 0.5]), relate(&line1, &line2).unique_intersection());
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
