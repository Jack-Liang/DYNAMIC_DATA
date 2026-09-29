# Changelog

All notable changes to this project are documented in this file.

## 3.2.0 - 2026-09-29

### Added

- `TO_JSON( data, name_map )`: serializes a generated (or arbitrary)
  dynamic data object back to JSON - the inverse of
  `CREATE_DATA_BY_JSON`. Rules:
  - `C` length 1 serializes as `true`/`false` (the shape the JSON
    type inference produces),
  - character-like initial values serialize as `null` (a data object
    cannot distinguish `null` from an empty string - `""` therefore
    round-trips as `null`),
  - numeric zeros are emitted as numbers (`0` is a value, not null),
  - the packed -> character conversion quirks are normalized (the
    trailing sign blank is condensed, a trailing minus moves to the
    front),
  - dates/times serialize as strings in their internal format
    (`20260929`), which round-trips through the filler; ISO conversion
    remains a future enhancement,
  - references and non-standard (sorted/hashed) tables raise
    `UNSUPPORTED_TYPE`; an unbound reference serializes as `null`.
- `NAME_MAP` is applied in reverse (ABAP component name -> JSON key),
  so over long keys survive a full json -> data -> json round trip.

### Tests

- 14 new tests in `ltcl_serialize`: exact-text rule tests (booleans,
  null/empty, zeros and negative packed numbers, escaping incl. non
  ASCII built from UTF-8 bytes, D/T internal format, empty tables,
  name map, unsupported types) and round-trip tests comparing the
  parsed node tables of input and output.
- Known asymmetry (pre-existing, documented): root arrays of scalars
  are built as `TABLE OF string` (walk_array only types member
  arrays), so `[10, 20]` at the root round-trips as strings.

## 3.1.0 - 2026-09-28

### Changed

- Tree based type building (restart of the reverted 2.2.0): the flat
  field description rows are parsed into a node tree and the types are
  generated bottom up. This removes the call stack depth detection
  (`SYSTEM_CALLSTACK`), the flag consumption, the parent reordering
  pass and the static `GT_FIELD_TAB` buffer - the class is now fully
  stateless between calls.
- The 2.2.0 on-system failure (`CX_SY_STRUCT_ATTRIBUTES` "The
  component table is empty") is root-caused and fixed on four counts:
  1. `lr_parent` was not cleared per source row, so nodes were
     mis-parented under the previous row's last node and structures
     lost their children,
  2. the nested struct create had no empty fallback (an empty array
     member triggered the exception),
  3. `sy-tabix` was clobbered by the `READ TABLE` on the path map
     inside the segment loop (replaced by an explicit counter),
  4. `boolc( )` returns a blank for false and comparing it against
     `abap_false` ignores trailing blanks only on the kernel - the
     builder now tests positively against `abap_true`.

### Added

- Undeclared intermediate path segments (e.g. only `A-B` configured,
  no `A` row) default to structures instead of leaking the child into
  the parent level.
- Invalid field names and empty struct nodes in `FIELD_TAB` now raise
  `EXECUTION_FAILED` instead of dumping in the RTTS type creation;
  rows with `FLAG` set are no longer silently skipped.
- Fixed on-system activation error: the builder error is raised via
  the concrete local class `LCX_BUILDER_ERROR` - `CX_DYNAMIC_CHECK`
  is abstract on real systems and cannot be instantiated (the
  transpiled CI does not enforce abstract instantiation).

### Tests

- Recovered from 2.2.0: `implicit_parent_becomes_struct`,
  `flat_pipeline_diagnosis`. New: `field_tab_out_of_order` (pins the
  mis-parenting fix), `empty_struct_node_raises`.
- The depth-2 nesting tests now also run in the transpiled CI (the
  call stack hack they were skipped for is gone).

## 3.0.0 - 2026-09-28

### Breaking changes

- `CREATE_MAIN` and `CREATE_DATA` are replaced by focused methods with
  parameter sets that fully apply to each path:
  - `CREATE_BY_FIELD_TAB( field_tab, type )` - type from a field
    description table, raises `UNSUPPORTED_TYPE` unless the root type
    is `S` or `T`
  - `CREATE_BY_JSON( json_data, infer_types, name_map )` - type from JSON
  - `CREATE_DATA_BY_JSON( json_data, infer_types, name_map )` - filled
    data object from JSON in one step
- `NO_TYPE` (double negative) is replaced by `INFER_TYPES`
  (`abap_false` by default = everything string, same behavior as before)
- empty or blank JSON now raises `INVALID_JSON` instead of
  `UNSUPPORTED_TYPE`; the JSON methods no longer declare
  `UNSUPPORTED_TYPE`
- `TY_SPLIT` moved from the public to the private section

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
