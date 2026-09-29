# TODO / Backlog

> 基线：v3.0.0（2026-09-28）——API 已拆分为 `CREATE_BY_FIELD_TAB` / `CREATE_BY_JSON` / `CREATE_DATA_BY_JSON`，
> `INFER_TYPES` / `NAME_MAP` 就绪，40 个单元测试 + demo 已在真实系统验证全绿。
> 排序即建议的执行顺序；①是②的前置条件。

## 1. abap-transpiler 进 CI：让 ABAP 单测在 GitHub Actions 里跑

- **背景**：目前 CI 只有 abaplint（静态检查）。多轮真机验证（pull → 激活 → SE80 跑测试）成本很高，
  本项目历史上多个 bug（CP 通配符、LENGTH 语义、静态表污染）都是靠真机测试才定位的。
- **参考**：[sbcgua/ajson](https://github.com/sbcgua/ajson) 的 `transpile_for_testing.json` + `bin/test.sh`，
  工具链为 npm 包 `@abaplint/transpiler-cli` + `@abaplint/runtime`，lib 指向 [open-abap/open-abap-core](https://github.com/open-abap/open-abap-core)
  （transpiler 原仓库 larshp/abap-transpiler 已迁移，旧链接 404）。
- **spike 已完成（2026-09-28，本地 Node 24）**，结论：
  - **可行**。18 个源文件全部转译通过（零语法错误、无 unknownTypes 报错），
    全套 37 个测试方法中 **34 个绿**，包括全部 6 个 sXML parser 测试——
    open-abap 的 sXML（`cl_sxml_string_reader` 纯 ABAP 实现）覆盖本项目用到的所有 API。
  - **阻塞 1（3 个测试挂）**：`cl_abap_elemdescr=>get_p` 在 open-abap 是 `ASSERT 1 = 'todo'` 占位，
    影响所有 P 类型推断测试（`packed_inferred` / `table_line_typed_from_tab` / `create_data_inferred_values`）。
    治本方案是给 open-abap-core 提 PR 实现 get_p；过渡方案按 ajson 的 `skip` 机制先跳过。
  - **阻塞 2（静默，无测试挂）**：`SYSTEM_CALLSTACK` 在 open-abap 有 stub 但只返回一行假帧，
    `structural_sub` 的递归深度恒为 0。现有测试最深只有一层嵌套（恰好不受影响，全绿），
    但**两层及以上嵌套在 CI 里会静默漏建字段**——当前测试套件没有两层嵌套用例，这是个覆盖缺口。
  - **缺 3 个 SAP 内置 DTEL**：`INTTYPE` / `ILEN` / `DECIMALS` open-abap 未收录（unknownTypes 报错）。
    spike 用 stub DTEL（char1 / numc6 / numc2）验证可行；正式方案：提给 open-abap-core 上游，
    或 CI 时在临时目录合并 stub（不能进仓库 src，真机 abapGit 导入会与 SAP 内置对象冲突）。
- **剩余步骤**：写 `.github/workflows/test.yml`（npm install → abap_transpile → node transpiled/index.mjs），
  处理上述三个阻塞（skip / 上游 PR），合入后删除 spike 临时物（`%TEMP%/zdoe-spike/`）。
- **完成标准**：push 后 CI 自动执行 `zcl_dynamic_object` 的 ABAP Unit 并保持绿色（或明确 skip 清单）。
- **进度（2026-09-28）**：CI 基建已落地并本地端到端验证（`sh bin/test.sh`：34 绿 / 3 skip / exit 0）——
  `bin/test.sh`（src+stub 合并到 `ci-build/` 后转译执行）、`ci/dtel-stubs/`（3 个 stub DTEL）、
  `transpile_for_testing.json`（含 skip 清单）、`package.json`（锁定 2.13.93）、
  `.github/workflows/test.yml`、`.gitignore`。
  **首跑已绿（2026-09-28）**：main（7ef9b0c）与 feature/tree-refactor（5eb4d88）上的
  abaplint + unit-tests 四个运行全部 success，本项完成。
  遗留跟进：~~向 open-abap-core 提 `get_p` 实现的 PR~~ **已提交并合入上游
  [open-abap-core#1275](https://github.com/open-abap/open-abap-core/pull/1275)（2026-09-28 合入）**。
  上游生效后已删除全部 3 个 get_p skip（2026-09-29 验证：44 用例全跑、0 skip、exit 0）——
  CI 测试覆盖无缺口，本项彻底完成。

## 2. 树形类型构建重构（重启 2.2.0 失败的尝试）

- **背景/动机**：一次性消灭四个 hack——
  1. `structural_sub` 用 `SYSTEM_CALLSTACK` 数栈帧推算递归深度（过滤串现在是 `'BUILD_FROM_ROWS'`）；
  2. `flag` 列边遍历边打标；
  3. `put_parent_field_first` 的父字段重排；
  4. 静态缓冲 `gt_field_tab`（**已经两次引发跨调用污染 bug**：漏写 CLEAR、去重误判）。
- **前置条件**：先完成 #1（本地能跑单测）。2.2.0 当时在真机上抛
  `CX_SY_STRUCT_ATTRIBUTES: The component table is empty` 且与源码矛盾、无法远程定位，最终回退
  （见 CHANGELOG 2.2.1）。
- **新增前置（spike 发现）**：重构前先补一个**两层嵌套 JSON 的测试用例**并确认真机绿——
  当前套件最深只有一层嵌套，而 CI 里 `SYSTEM_CALLSTACK` stub 使深度恒为 0，
  两层嵌套路径目前既无真机用例覆盖、CI 也测不准（会静默漏字段）。重构恰好要删掉这个栈帧 hack，
  这个用例同时守护重构前后行为。
  **进度（2026-09-28，分支 `feature/tree-refactor`）**：已补 3 个用例
  （`deep_nested_struct` / `deep_nested_table_in_table` / `create_data_deep_nested`），
  本地验证：open-abap 里如预期失败（深度恒 0）→ 已加 CI skip
  （注明"删掉栈帧 hack 后移除"）。**真机已验证绿（2026-09-28）**——前置达成，可开始重构。
- **线索**：树形 IR 方向本身没错；怀疑点集中在"本地类引用作 RETURNING 表 + LOOP INTO ref + 解引用"
  链路的某个 ABAP 语义。有本地测试后按二分法定位。
  **考古结论（2026-09-28，git 历史 + 本地 CI 复现）**：与引用链路无关，真凶是四个叠加缺陷——
  ①`lr_parent` 未按行清零（节点错挂）；②嵌套 struct create 无空回退（`empty_array_member` 必触发）；
  ③段循环内 `sy-tabix` 被 `READ TABLE` 改写（内核上同为未定义行为）；④`boolc(false)` 返回空格，
  `' ' = abap_false` 仅内核忽略尾部空格、open-abap 判 false（全部节点被误判 implicit 的直接原因）。
  四项均已修复并有回归测试钉死。
- **完成标准**：四个 hack 全部删除、类在调用之间完全无状态、全部单测绿、行为不变。
  **进度（2026-09-28，本地全绿）**：重构完成——`lcl_tree_node`/`lcl_type_builder`（含四修复）进 locals_imp，
  主类删掉 `gt_field_tab`/`build_from_rows`/`structural_sub`/`put_parent_field_first`/`append_field`
  （596→288 行），44 用例 41 跑全绿 + 3 skip（get_p），深度嵌套三连在 CI 转绿（skip 已删），abaplint 0 issue。
  行为差异（有意为之，见 CHANGELOG 3.1.0）：缺失父段→隐式 struct；非法名/空 struct 节点→`execution_failed`
  而非 dump；带 `flag` 的入参行不再被丢弃。**待真机验证后合并**。

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
