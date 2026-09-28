# TODO / Backlog

> 基线：v3.0.0（2026-09-28）——API 已拆分为 `CREATE_BY_FIELD_TAB` / `CREATE_BY_JSON` / `CREATE_DATA_BY_JSON`，
> `INFER_TYPES` / `NAME_MAP` 就绪，40 个单元测试 + demo 已在真实系统验证全绿。
> 排序即建议的执行顺序；①是②的前置条件。

## 1. abap-transpiler 进 CI：让 ABAP 单测在 GitHub Actions 里跑

- **背景**：目前 CI 只有 abaplint（静态检查）。多轮真机验证（pull → 激活 → SE80 跑测试）成本很高，
  本项目历史上多个 bug（CP 通配符、LENGTH 语义、静态表污染）都是靠真机测试才定位的。
- **参考**：[sbcgua/ajson](https://github.com/sbcgua/ajson) 的 `transpile_for_testing.json` +
  `.github/workflows/test.yml`，基于 [larshp/abap-transpiler](https://github.com/larshp/abap-transpiler)。
- **先做 spike**：transpiler 对内核类 `cl_sxml_string_reader`（sXML）是否有模拟层是最大风险点，
  先跑通一个现有测试用例（例如 `ltcl_json_parser=>parse_object`）验证可行性，再铺全量。
- **完成标准**：push 后 CI 自动执行 `zcl_dynamic_object` 的 ABAP Unit 并保持绿色。

## 2. 树形类型构建重构（重启 2.2.0 失败的尝试）

- **背景/动机**：一次性消灭四个 hack——
  1. `structural_sub` 用 `SYSTEM_CALLSTACK` 数栈帧推算递归深度（过滤串现在是 `'BUILD_FROM_ROWS'`）；
  2. `flag` 列边遍历边打标；
  3. `put_parent_field_first` 的父字段重排；
  4. 静态缓冲 `gt_field_tab`（**已经两次引发跨调用污染 bug**：漏写 CLEAR、去重误判）。
- **前置条件**：先完成 #1（本地能跑单测）。2.2.0 当时在真机上抛
  `CX_SY_STRUCT_ATTRIBUTES: The component table is empty` 且与源码矛盾、无法远程定位，最终回退
  （见 CHANGELOG 2.2.1）。
- **线索**：树形 IR 方向本身没错；怀疑点集中在"本地类引用作 RETURNING 表 + LOOP INTO ref + 解引用"
  链路的某个 ABAP 语义。有本地测试后按二分法定位。
- **完成标准**：四个 hack 全部删除、类在调用之间完全无状态、全部单测绿、行为不变。

## 3. 反向序列化 to_json（ABAP → JSON）

- 与 `CREATE_DATA_BY_JSON` 对称：对生成的（或任意动态）数据对象做序列化；
  bool `X`→true、初始→null、数字按类型格式化。参考 ajson 的 serializer。
- **完成标准**：round-trip 单测（json → data → json 语义等价）。

## 4. 可选增强（按需）

- `field_tab` 路径一步式 API（`CREATE_DATA_BY_FIELD_TAB`，仅 CREATE DATA 不填值）——价值低，有用户需求再做。
- 经典异常 → 类异常（`RAISING zcx_...`）API 变体：让 `INVALID_FIELD_NAME` 能带出具体是哪个 key
  （文本目前在 `zcx_dynamic_name_error->text` 里被丢弃）；需 major 版本窗口。
- `build_elem_descr` 补 `b`/`s`/`y`/`8`（int1/int2/xstring/int8），目前落入 `describe_by_name` 会运行时报错。
- 深层 JSON 性能：`LOOP AT nodes WHERE path =` 是 O(n²) 路径扫描，大 JSON 时加 sorted key 优化。
- 推断增强：ISO 日期/时间字符串 → `D`/`T`。
- abaplint 收紧规则（indentation / functional_writing 目前关闭，需先统一缩进风格）。
