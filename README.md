# mpd-filters

[![ci](https://github.com/tsirysndr/mpd-filters/actions/workflows/ci.yml/badge.svg)](https://github.com/tsirysndr/mpd-filters/actions/workflows/ci.yml)
[![downloads](https://img.shields.io/crates/dr/mpd-filters)](https://crates.io/crates/mpd-filters)
[![crates](https://img.shields.io/crates/v/mpd-filters.svg)](https://crates.io/crates/mpd-filters)

A simple mpd query parser for Rust.

## Usage

```rust
let mut parser = Parser::new("Album == '10 Summers' AND Artist == 'DJ Mustard'");
match parser.parse() {
  Ok(expr) => println!("Parsed expression: {:?}\n sql: {:?}", expr, expr.to_sql(SqlOptions::default())),
  Err(e) => panic!("Error parsing: {}", e),
}
```

