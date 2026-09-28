# Changelog

All notable changes to this project are documented in this file.

## 2.2.0 - 2026-09-28

### Changed

- Tree based type building: the flat field description rows are parsed
  into a node tree first and the types are generated bottom up. This
  removes the call stack depth detection (`SYSTEM_CALLSTACK`), the flag
  consumption logic, the parent reordering pass and the static
  `GT_FIELD_TAB` buffer - the class is now stateless between calls.
- `STRUF` now uses the self built data element `ZDOE_STRUF` (string)
  instead of the CRM data element `CRM_OST_REF_FIELD`, removing the
  activation risk on systems without CRM components. Values like
  `SFLIGHT-CARRID` keep working, no length limit applies.
- Undeclared intermediate path segments (e.g. only `A-B` configured,
  no `A` row) now default to structures instead of leaking the child
  into the parent level.
- Invalid field names in `FIELD_TAB` now raise `EXECUTION_FAILED`
  instead of dumping in the RTTS type creation.

## 2.1.0 - 2026-09-28

### Added

- `CREATE_DATA`: one step JSON -> generated type plus filled data object.
  The JSON is parsed once with the kernel sXML library; both type
  generation and value population use the same node table, so no JSON
  binder (e.g. `/ui2/cl_json`) is needed on the caller side. Booleans
  become `X`/initial, `null` stays initial.
- Demo report section `Usage 4` showing the filled values.

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
- Metadata constants `ZCL_DYNAMIC_OBJECT=>C_INFO` (version, author,
  email, repository, license) for runtime self-description.
- abaplint configuration (`abaplint.jsonc`, clean) and a GitHub Actions
  workflow running it.
