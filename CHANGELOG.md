# Changelog

All notable changes to this project are documented in this file.

## 2.3.0 - 2026-09-28

### Added

- `NAME_MAP` parameter for `CREATE_MAIN` and `CREATE_DATA`: maps over
  long or otherwise invalid JSON keys to valid ABAP component names.
  Entries are matched case insensitively; a mapped target that itself
  violates the ABAP name rules raises `INVALID_FIELD_NAME`. Type
  generation and value population (`CREATE_DATA`) use the same mapping.
- Demo report section `Usage 5` and four new unit tests.

## 2.2.1 - 2026-09-28

### Fixed

- **Length inference bug (the long standing "len 2" mystery)**: the sign
  check `CP '+*'` treated `+` as a single character wildcard, so every
  integer part lost its first digit (`88` became `8`) and packed lengths
  came out too small. Replaced with an explicit first character compare.
- Restored the proven 2.1.0 type building pipeline. The tree based
  builder from 2.2.0 failed on-system with
  `CX_SY_STRUCT_ATTRIBUTES` ("The component table is empty") in a way
  that contradicts the source code, so it is reverted until tests can
  run locally in CI (abap-transpiler). The `ZDOE_STRUF` data element,
  the sXML based walker/filler and the diagnostic unit tests are kept.
- Tests and the demo now measure character type lengths with
  `DESCRIBE FIELD ... IN CHARACTER MODE`: the RTTS `->length` attribute
  reports bytes for character types on some releases (a correct C(1)
  shows as 2 in UCS-2 systems).

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

### Fixed

- Tree nodes carried the full field path (e.g. `SKILLS-NAME`) as the
  component name instead of the single path segment, which made
  `CL_ABAP_STRUCTDESCR=>CREATE` reject every nested structure.
- New diagnostic unit test `FLAT_PIPELINE_DIAGNOSIS` reports the exact
  exception class and text if the minimal flat build fails.

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
