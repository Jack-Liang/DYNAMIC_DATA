# Changelog

All notable changes to this project are documented in this file.

## 2.0.0 - 2026-09-28

### Breaking / behavior changes

- JSON is now parsed with the kernel `sXML` library into an intermediate
  node table (parsing approach borrowed from [sbcgua/ajson](https://github.com/sbcgua/ajson),
  MIT). The `/ui2/cl_json` dependency is no longer used for type generation.
- `CREATE_MAIN` raises two new classic exceptions: `INVALID_JSON` and
  `INVALID_FIELD_NAME` (see README for the field name rules).
- JSON keys containing `-`, invalid characters, or longer than 30 characters
  are rejected instead of silently producing broken or missing components.

### Fixed

- Elementary `INTTY` specifications are built via RTTS factories
  (`CL_ABAP_ELEMDESCR=>GET_P` etc.) with explicit `CONV i`. Passing the
  text-like DDIC fields `LENGT`/`DECIM` into `CREATE DATA ... LENGTH`
  produced wrong lengths (e.g. `C` always came out as length 2).
- Fields with initial values (`0`, `""`) are no longer dropped when
  generating from JSON.
- A root level empty JSON array (`[]`) no longer dumps; it produces
  `TABLE OF string`.
- The duplicate field check now runs after upper case normalization,
  so `hello` + `HELLO` is correctly detected as a duplicate.
- The empty table fallback (`"xxx": []`) now also returns `ref_type`
  instead of only `ref_data`.

### Added

- Type inference (`NO_TYPE = ''`): small integers become `i`, decimals
  become `p` sized from the literal, booleans become `c(1)`; arrays of
  consistent scalar items type the table line accordingly; different keys
  across array items are unioned into the line structure.
- Field description rows with `FLDTYPE = 'T'` and `INTTY`/`LENGT`/`DECIM`
  now generate a table with an elementary line type.
- ABAP Unit test suite (parser, walker and `CREATE_MAIN` black box tests
  including regressions for all fixes above).
- Demo / smoke test report `ZDYNAMIC_DATA_DEMO`.
- abaplint configuration (`abaplint.jsonc`, clean) and a GitHub Actions
  workflow running it.
