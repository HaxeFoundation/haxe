# Version History

## dev branch / next version (1.x.x)

## version 1.2.18 (2024-11-01)

- Fixed TokenTreeCheckUtils.getBrOpenType detection of pattern extraction block

## version 1.2.17 (2024-10-04)

- Fixed nested conditionals

## version 1.2.16 (2024-10-03)

- Fixed conditionals after PClose

## version 1.2.15 (2024-10-02)

- Fixed operator keyword used in Haxe 3 code

## version 1.2.14 (2024-09-18)

- Fixed getArrowType running into a null pointer exception with conditionals

## version 1.2.13 (2024-08-02)

- Fixed metadata for parens

## version 1.2.12 (2024-07-30)

- Fixed parsing a single `false`, `true` or `null`

## version 1.2.11 (2024-02-10)

- Added getBkOpenType utils function
- Fixed empty switch cases with comments

## version 1.2.10 (2023-05-21)

- Fixed extends handling in interfaces

## version 1.2.9 (2023-05-21)

- Fixed unicode in regexp, fixes [#219](https://github.com/HaxeCheckstyle/tokentree/issues/219)

## version 1.2.8 (2023-02-22)

- Fixed inline call() and inline new handling ([#217](https://github.com/HaxeCheckstyle/tokentree/issues/217))

## version 1.2.7 (2023-02-15)

- Fixed type parameter detection ([#216](https://github.com/HaxeCheckstyle/tokentree/issues/216))

## version 1.2.6 (2023-02-06)

- Fixed pos info of Root nodes, fixes [#215](https://github.com/HaxeCheckstyle/tokentree/issues/215)

## version 1.2.5 (2022-12-03)

- Fixed ternary detection ([#214](https://github.com/HaxeCheckstyle/tokentree/issues/214))

## version 1.2.4 (2022-12-01)

- Added findLastBinop utils function ([#213](https://github.com/HaxeCheckstyle/tokentree/issues/213))
- Fixed macro type hint ([#213](https://github.com/HaxeCheckstyle/tokentree/issues/213))
- Fixed enum type parameter ([#213](https://github.com/HaxeCheckstyle/tokentree/issues/213))

## version 1.2.3 (2022-09-14)

- Fixed suffixes for negative numeric literals

## version 1.2.2 (2022-09-14)

- Updated haxeparser to support latest Haxe nightly syntax

## version 1.2.1 (2022-09-07)

- Moved inline markup code to haxeparser ([#212](https://github.com/HaxeCheckstyle/tokentree/issues/212))

## version 1.2.0 (2022-08-27)

- Added support for inline markup ([#211](https://github.com/HaxeCheckstyle/tokentree/issues/211))

## version 1.1.2 (2022-04-23)

- Added support for default type parameter ([#207](https://github.com/HaxeCheckstyle/tokentree/issues/207))
- Added support for static locals ([#208](https://github.com/HaxeCheckstyle/tokentree/issues/208) + [#209](https://github.com/HaxeCheckstyle/tokentree/issues/209))
- Fixed Dot handling for new Class() instances ([#210](https://github.com/HaxeCheckstyle/tokentree/issues/210))

## version 1.1.1 (2021-01-22)

- Fixed POpen after block curlies ([#206](https://github.com/HaxeCheckstyle/tokentree/issues/206))

## version 1.1.0 (2020-12-23)

- Retired Haxe 3.4.7 compile support ([#202](https://github.com/HaxeCheckstyle/tokentree/issues/202))
- Upgraded to latest haxeparser version ([#202](https://github.com/HaxeCheckstyle/tokentree/issues/202) + [#204](https://github.com/HaxeCheckstyle/tokentree/issues/204))
- Added testcases for ExpressionLevel code ([#202](https://github.com/HaxeCheckstyle/tokentree/issues/202))
- Added stream end detection to POpen and array access parsing ([#202](https://github.com/HaxeCheckstyle/tokentree/issues/202))
- Added overload support ([#204](https://github.com/HaxeCheckstyle/tokentree/issues/204))
- Fixed function type parameter ([#203](https://github.com/HaxeCheckstyle/tokentree/issues/203))

## version 1.0.31 (2020-11-04)

- Fixed type parameter with array ([#201](https://github.com/HaxeCheckstyle/tokentree/issues/201))

## version 1.0.30 (2020-11-02)

- Fixed arrow functions ([#200](https://github.com/HaxeCheckstyle/tokentree/issues/200))
- Fixed type parameter detection ([#200](https://github.com/HaxeCheckstyle/tokentree/issues/200))
- Fixed Dollar handling ([#200](https://github.com/HaxeCheckstyle/tokentree/issues/200))

## version 1.0.29 (2020-11-01)

- Added TokenTreeDef ([#191](https://github.com/HaxeCheckstyle/tokentree/issues/191))
- Added support for abstract classes / interfaces ([#195](https://github.com/HaxeCheckstyle/tokentree/issues/195))
- Added hasTempStore function ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Fixed negative const / Binop(OpSub) detection ([#189](https://github.com/HaxeCheckstyle/tokentree/issues/189))
- Fixed final with multiple vars ([#191](https://github.com/HaxeCheckstyle/tokentree/issues/191))
- Fixed is operator ([#194](https://github.com/HaxeCheckstyle/tokentree/issues/194))
- Fixed semicolon with multiple binops ([#197](https://github.com/HaxeCheckstyle/tokentree/issues/197) + [#198](https://github.com/HaxeCheckstyle/tokentree/issues/198))
- Fixed Dollar handling ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Fixed expression handling after BkClose ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Fixed expression handling after BrClose ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Fixed POpen type detection ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Fixed new handling ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Fixed sharp handling for function type hint ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Fixed macro complextype ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Fixed array access on new ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Fixed arrow functions ([#199](https://github.com/HaxeCheckstyle/tokentree/issues/199))
- Refactored enums to use CamelCase ([#191](https://github.com/HaxeCheckstyle/tokentree/issues/191))
- Refactored to reduce usage of Type.enumEq ([#195](https://github.com/HaxeCheckstyle/tokentree/issues/195) + [#196](https://github.com/HaxeCheckstyle/tokentree/issues/196))
- Removed `is` operator ([#195](https://github.com/HaxeCheckstyle/tokentree/issues/195))

## version 1.0.28 (2020-06-07)

- Expanded `POpen` types in getPOpenType to distinguish between different condition expressions ([#185](https://github.com/HaxeCheckstyle/tokentree/issues/185))
- Fixed type hint detection for anon types ([#186](https://github.com/HaxeCheckstyle/tokentree/issues/186))
- Fixed final position for Haxe 3 ([#186](https://github.com/HaxeCheckstyle/tokentree/issues/186))
- Fixed @in handling ([#187](https://github.com/HaxeCheckstyle/tokentree/issues/187))

## version 1.0.27 (2020-04-18)

- Fixed null pointer exception when pasrsing `private final class`, fixes [#183](https://github.com/HaxeCheckstyle/tokentree/issues/183) ([#184](https://github.com/HaxeCheckstyle/tokentree/issues/184))

## version 1.0.26 (2020-04-12)

- Fixed `Binop(OpOr)` position for non `case` places ([#182](https://github.com/HaxeCheckstyle/tokentree/issues/182))
- Fixed doc comments and metadata ([#182](https://github.com/HaxeCheckstyle/tokentree/issues/182))

## version 1.0.25 (2020-04-11)

- Added support for module level statics [#175](https://github.com/HaxeCheckstyle/tokentree/issues/175)
- Added final handling for interfaces [#176](https://github.com/HaxeCheckstyle/tokentree/issues/176)
- Fixed unary `OpSub` handling [#174](https://github.com/HaxeCheckstyle/tokentree/issues/174)
- Fixed arrow function detection [#175](https://github.com/HaxeCheckstyle/tokentree/issues/175) + [#177](https://github.com/HaxeCheckstyle/tokentree/issues/177)
- Fixed `POpen` type detection [#177](https://github.com/HaxeCheckstyle/tokentree/issues/177) + [#180](https://github.com/HaxeCheckstyle/tokentree/issues/180)
- Fixed `BrOpen` type detection [#178](https://github.com/HaxeCheckstyle/tokentree/issues/178)
- Fixed `Binop(OpOr)` position [#178](https://github.com/HaxeCheckstyle/tokentree/issues/178)
- Fixed expression metadata position [#179](https://github.com/HaxeCheckstyle/tokentree/issues/179)
- Fixed `Binop(OpBoolOr)` and `Binop(OpBoolAnd)` positions [#180](https://github.com/HaxeCheckstyle/tokentree/issues/180)

## version 1.0.24 (2019-12-01)

- Added comment handling after `PClose` [#172](https://github.com/HaxeCheckstyle/tokentree/issues/172)
- Added entry point TYPE_HINT_LEVEL, fixes [#147](https://github.com/HaxeCheckstyle/tokentree/issues/147)
- Fixed semicolon handling after null, true or false [#171](https://github.com/HaxeCheckstyle/tokentree/issues/171)
- Refactored build system to use lix [#173](https://github.com/HaxeCheckstyle/tokentree/issues/173)

## version 1.0.23 (2019-09-10)

- Fixed postfix exclamation mark [#165](https://github.com/HaxeCheckstyle/tokentree/issues/165)
- Fixed C# exception handling [#166](https://github.com/HaxeCheckstyle/tokentree/issues/166)
- Fixed null pointer issues in TokenTreeCheckUtils [#167](https://github.com/HaxeCheckstyle/tokentree/issues/167)
- Fixed endless loop in for loops with macro reification, fixes [#168](https://github.com/HaxeCheckstyle/tokentree/issues/168) ([#169](https://github.com/HaxeCheckstyle/tokentree/issues/169))

## version 1.0.22 (2019-06-13)

- Fixed type hint detection in enums and overload metas [#161](https://github.com/HaxeCheckstyle/tokentree/issues/161)
- Fixed comments in OpAdd chains [#162](https://github.com/HaxeCheckstyle/tokentree/issues/162)
- Fixed imports with conditionals [#163](https://github.com/HaxeCheckstyle/tokentree/issues/163)

## version 1.0.21 (2019-06-01)

- Fixed conditionals using dot idents without parens [#159](https://github.com/HaxeCheckstyle/tokentree/issues/159)

## version 1.0.20 (2019-05-17)

- Fixed potential null pointer in `TokenTreeAccessHelper.findParent` [#157](https://github.com/HaxeCheckstyle/tokentree/issues/157)

## version 1.0.19 (2019-05-17)

- Added `TokenTreeAccessHelper.findParent` [#150](https://github.com/HaxeCheckstyle/tokentree/issues/150)
- Added a cache for getArrowType, getBrOpenType, getColonType and getPOpenType results [#155](https://github.com/HaxeCheckstyle/tokentree/issues/155)
- Fixed `TokenTreeCheckUtils.getColonType` detection of type checks [#149](https://github.com/HaxeCheckstyle/tokentree/issues/149)
- Fixed `OpBool` position in case body [#151](https://github.com/HaxeCheckstyle/tokentree/issues/151)
- Fixed object literal detection when using expression entry point [#152](https://github.com/HaxeCheckstyle/tokentree/issues/152) + [#153](https://github.com/HaxeCheckstyle/tokentree/issues/153)
- Fixed `TokenTreeCheckUtils.getColonType` in if body [#154](https://github.com/HaxeCheckstyle/tokentree/issues/154)
- Fixed `TokenTreeCheckUtils.getPOpenType` in if body [#154](https://github.com/HaxeCheckstyle/tokentree/issues/154)

## version 1.0.18 (2019-03-22)

- Added `TokenTreeEntryPoint` to `TokenTreeBuilder` [#146](https://github.com/HaxeCheckstyle/tokentree/issues/146)

## version 1.0.17 (2019-03-06)

- Fixed wrong position of elements preceeding inserted dummy tokens in relax mode [#145](https://github.com/HaxeCheckstyle/tokentree/issues/145)

## version 1.0.16 (2019-03-04)

- Added `TokenTreeCheckUtils.isMetadata` [#143](https://github.com/HaxeCheckstyle/tokentree/issues/143)
- Fixed handling of OpBool after `null` [#140](https://github.com/HaxeCheckstyle/tokentree/issues/140)
- Fixed comments before `catch` [#140](https://github.com/HaxeCheckstyle/tokentree/issues/140)
- Fixed BrOpen detection for type parameter with structure type [#140](https://github.com/HaxeCheckstyle/tokentree/issues/140)
- Fixed colon type detection when using conditionals [#140](https://github.com/HaxeCheckstyle/tokentree/issues/140)
- Fixed colon type detection when using OpBool [#140](https://github.com/HaxeCheckstyle/tokentree/issues/140)
- Fixed handling of condition bodies (if, while, for) [#141](https://github.com/HaxeCheckstyle/tokentree/issues/141)
- Fixed POpen type detection for conditionals [#141](https://github.com/HaxeCheckstyle/tokentree/issues/141)
- Fixed comments in abstracts `from` and `to` [#142](https://github.com/HaxeCheckstyle/tokentree/issues/142)
- Fixed arrow type detection with comments [#142](https://github.com/HaxeCheckstyle/tokentree/issues/142)
- Fixed position of `OpAdd` / `OpSub` [#142](https://github.com/HaxeCheckstyle/tokentree/issues/142)
- Fixed detection of "unary" `OpSub` [#142](https://github.com/HaxeCheckstyle/tokentree/issues/142)

## version 1.0.15 (2019-02-07)

- Fixed `TokenTreeCheckUtils.getColonType` detection of type check in array comprehension [#136](https://github.com/HaxeCheckstyle/tokentree/issues/136)
- Fixed handling of multiple `implements` [#137](https://github.com/HaxeCheckstyle/tokentree/issues/137)
- Fixed comments after typedefs without semicolon [#138](https://github.com/HaxeCheckstyle/tokentree/issues/138)

## version 1.0.14 (2018-12-05)

- Fixed properties in anon types [#130](https://github.com/HaxeCheckstyle/tokentree/issues/130)
- Fixed `return` in ternary [#132](https://github.com/HaxeCheckstyle/tokentree/issues/132)
- Fixed handling of `;` [#133](https://github.com/HaxeCheckstyle/tokentree/issues/133) + [#134](https://github.com/HaxeCheckstyle/tokentree/issues/134)
- Refactored coverage report generation [#131](https://github.com/HaxeCheckstyle/tokentree/issues/131)

## version 1.0.13 (2018-11-11)

- Added testcase for multiline string interpolation [#126](https://github.com/HaxeCheckstyle/tokentree/issues/126)
- Fixed handling of OpBool (`||` and `&&`) chains [#124](https://github.com/HaxeCheckstyle/tokentree/issues/124)
- Fixed handling of Comma after `#end` [#124](https://github.com/HaxeCheckstyle/tokentree/issues/124)
- Fixed conditionals in interface definition [#125](https://github.com/HaxeCheckstyle/tokentree/issues/125)
- Fixed handling of Arrow in arrow functions [#127](https://github.com/HaxeCheckstyle/tokentree/issues/127)
- Fixed POpen detection in arrow functions [#127](https://github.com/HaxeCheckstyle/tokentree/issues/127)
- Fixed OpBool chain handling [#128](https://github.com/HaxeCheckstyle/tokentree/issues/128)

## version 1.0.12 (2018-10-15)

- Added support for final class / interface, fixes [#118](https://github.com/HaxeCheckstyle/tokentree/issues/118) ([#119](https://github.com/HaxeCheckstyle/tokentree/issues/119))
- Added support for key-value iterators [#120](https://github.com/HaxeCheckstyle/tokentree/issues/120)
- Fixed handling of metadata with parameter [#121](https://github.com/HaxeCheckstyle/tokentree/issues/121)
- Fixed comments in method chains [#122](https://github.com/HaxeCheckstyle/tokentree/issues/122)

## version 1.0.11 (2018-09-03)

- Fixed position of semicolon [#113](https://github.com/HaxeCheckstyle/tokentree/issues/113)
- Fixed handling of unary expressions [#113](https://github.com/HaxeCheckstyle/tokentree/issues/113)
- Fixed detection of type check on numbers [#114](https://github.com/HaxeCheckstyle/tokentree/issues/114)
- Fixed detection of type parameters with anon types [#115](https://github.com/HaxeCheckstyle/tokentree/issues/115)
- Fixed handling of typedef without assign [#116](https://github.com/HaxeCheckstyle/tokentree/issues/116)

## version 1.0.10 (2018-08-20)

- Added `TokenTreeCheckUtils.getColonType()` [#99](https://github.com/HaxeCheckstyle/tokentree/issues/99) + [#100](https://github.com/HaxeCheckstyle/tokentree/issues/100) + [#101](https://github.com/HaxeCheckstyle/tokentree/issues/101)
- Fixed implements / extends handling [#94](https://github.com/HaxeCheckstyle/tokentree/issues/94)
- Fixed endless loop with macro patterns [#94](https://github.com/HaxeCheckstyle/tokentree/issues/94)
- Fixed macro class $name [#94](https://github.com/HaxeCheckstyle/tokentree/issues/94)
- Fixed conditionals in new [#95](https://github.com/HaxeCheckstyle/tokentree/issues/95)
- Fixed object literal detection [#96](https://github.com/HaxeCheckstyle/tokentree/issues/96)
- Fixed `getBrOpenType` detection of blocks [#96](https://github.com/HaxeCheckstyle/tokentree/issues/96) + [#99](https://github.com/HaxeCheckstyle/tokentree/issues/99) + [#107](https://github.com/HaxeCheckstyle/tokentree/issues/107) + [#110](https://github.com/HaxeCheckstyle/tokentree/issues/110)
- Fixed `getArrowType` detection [#97](https://github.com/HaxeCheckstyle/tokentree/issues/97) + [#110](https://github.com/HaxeCheckstyle/tokentree/issues/110) + [#111](https://github.com/HaxeCheckstyle/tokentree/issues/111)
- Fixed position of trailing comments in switch/cases [#98](https://github.com/HaxeCheckstyle/tokentree/issues/98)
- Fixed position of type checks on object literals [#99](https://github.com/HaxeCheckstyle/tokentree/issues/99)
- Fixed `getPOpenType` parameter detection [#100](https://github.com/HaxeCheckstyle/tokentree/issues/100) + [#101](https://github.com/HaxeCheckstyle/tokentree/issues/101) + [#104](https://github.com/HaxeCheckstyle/tokentree/issues/104)
- Fixed ternary position [#102](https://github.com/HaxeCheckstyle/tokentree/issues/102) + [#105](https://github.com/HaxeCheckstyle/tokentree/issues/105) + [#108](https://github.com/HaxeCheckstyle/tokentree/issues/108) + [#109](https://github.com/HaxeCheckstyle/tokentree/issues/109)
- Fixed structure extension position of `&` [#103](https://github.com/HaxeCheckstyle/tokentree/issues/103)
- Fixed `do…while` handling [#105](https://github.com/HaxeCheckstyle/tokentree/issues/105)
- Fixed "interesting" use of conditionals [#106](https://github.com/HaxeCheckstyle/tokentree/issues/106)
- Fixed arrow position in typedef to function type with structure argument [#108](https://github.com/HaxeCheckstyle/tokentree/issues/108)
- Fixed nested while loop detection [#108](https://github.com/HaxeCheckstyle/tokentree/issues/108)
- Fixed handling of typed parameters in structure extensions [#109](https://github.com/HaxeCheckstyle/tokentree/issues/109)

## version 1.0.9 (2018-08-13)

- Fixed endless loop with case and conditionals [#93](https://github.com/HaxeCheckstyle/tokentree/issues/93)

## version 1.0.8 (2018-08-13)

- Fixed null poiner and detection in `getPOpenType` [#73](https://github.com/HaxeCheckstyle/tokentree/issues/73) + [#88](https://github.com/HaxeCheckstyle/tokentree/issues/88) +  [#91](https://github.com/HaxeCheckstyle/tokentree/issues/91)
- Fixed property modifiers [#74](https://github.com/HaxeCheckstyle/tokentree/issues/74)
- Fixed function type detection [#74](https://github.com/HaxeCheckstyle/tokentree/issues/74)
- Fixed `getBrOpenType` detection of blocks [#75](https://github.com/HaxeCheckstyle/tokentree/issues/75) + [#77](https://github.com/HaxeCheckstyle/tokentree/issues/77) + [#79](https://github.com/HaxeCheckstyle/tokentree/issues/79) + [#81](https://github.com/HaxeCheckstyle/tokentree/issues/81) + [#83](https://github.com/HaxeCheckstyle/tokentree/issues/83) + [#86](https://github.com/HaxeCheckstyle/tokentree/issues/86) + [#87](https://github.com/HaxeCheckstyle/tokentree/issues/87) + [#88](https://github.com/HaxeCheckstyle/tokentree/issues/88)
- Fixed using multiple metadata with popen [#76](https://github.com/HaxeCheckstyle/tokentree/issues/76)
- Fixed support for @new [#78](https://github.com/HaxeCheckstyle/tokentree/issues/78)
- Fixed `isTernary` [#80](https://github.com/HaxeCheckstyle/tokentree/issues/80) + [#89](https://github.com/HaxeCheckstyle/tokentree/issues/89)
- Fixed `getArrowType` [#80](https://github.com/HaxeCheckstyle/tokentree/issues/80) + [#85](https://github.com/HaxeCheckstyle/tokentree/issues/85)
- Fixed comment detection in type hints [#82](https://github.com/HaxeCheckstyle/tokentree/issues/82)
- Fixed property handling in `getFieldType` [#84](https://github.com/HaxeCheckstyle/tokentree/issues/84)
- Fixed handling of typedef fields (@:overload, etc.) [#89](https://github.com/HaxeCheckstyle/tokentree/issues/89)
- Fixed ternary parsing [#90](https://github.com/HaxeCheckstyle/tokentree/issues/90)
- Fixed endless loop with switch and conditionals [#92](https://github.com/HaxeCheckstyle/tokentree/issues/92)

## version 1.0.7 (2018-07-26)

- Added `TokenTreeCheckUtils.isTypeStructure()` [#36](https://github.com/HaxeCheckstyle/tokentree/issues/36) + [#50](https://github.com/HaxeCheckstyle/tokentree/issues/50)
- Added `TokenTreeCheckUtils.isTypeEnum()` [#40](https://github.com/HaxeCheckstyle/tokentree/issues/40)
- Added `TokenTreeCheckUtils.isMacroClass()` [#42](https://github.com/HaxeCheckstyle/tokentree/issues/42)
- Added `TokenTreeCheckUtils.isOperatorFunction` [#44](https://github.com/HaxeCheckstyle/tokentree/issues/44)
- Added `TokenTreeCheckUtils.getMetadata()` and `isModifier()` [#44](https://github.com/HaxeCheckstyle/tokentree/issues/44)
- Added `TokenTreeCheckUtils.getBrOpenType()` and `getPOpenType()` [#51](https://github.com/HaxeCheckstyle/tokentree/issues/51)
- Added `TokenTreeCheckUtils.isDeprecated()` [#67](https://github.com/HaxeCheckstyle/tokentree/issues/67)
- Added `TokenTreeCheckUtils.getName()` [#68](https://github.com/HaxeCheckstyle/tokentree/issues/68)
- Added `TokenTreeCheckUtils.getNameToken()` [#68](https://github.com/HaxeCheckstyle/tokentree/issues/68)
- Added `TokenTreeCheckUtils.isNameToken()` [#68](https://github.com/HaxeCheckstyle/tokentree/issues/68)
- Added `TokenTreeCheckUtils.getDocComment()` [#69](https://github.com/HaxeCheckstyle/tokentree/issues/69)
- Added support for structural extension [#49](https://github.com/HaxeCheckstyle/tokentree/issues/49)
- Added support for `final` [#55](https://github.com/HaxeCheckstyle/tokentree/issues/55) + [#56](https://github.com/HaxeCheckstyle/tokentree/issues/56) + [#61](https://github.com/HaxeCheckstyle/tokentree/issues/61)
- Added `TokenTreeCheckUtils.getArrowType()` [#70](https://github.com/HaxeCheckstyle/tokentree/issues/70) +  [#71](https://github.com/HaxeCheckstyle/tokentree/issues/71)
- Fixed running out of tokens during package and imports [#28](https://github.com/HaxeCheckstyle/tokentree/issues/28)
- Fixed `function` in typedef body [#30](https://github.com/HaxeCheckstyle/tokentree/issues/30)
- Fixed handling of `switch this {` [#34](https://github.com/HaxeCheckstyle/tokentree/issues/34)
- Fixed handling of `macro class` [#37](https://github.com/HaxeCheckstyle/tokentree/issues/37)
- Fixed `isTernary` [#38](https://github.com/HaxeCheckstyle/tokentree/issues/38)
- Fixed handling `return if (cond) -1 else 0` [#46](https://github.com/HaxeCheckstyle/tokentree/issues/46)
- Fixed `Comma` in object declarations [#52](https://github.com/HaxeCheckstyle/tokentree/issues/52)
- Fixed `@:final` handling in Haxe 4 [#53](https://github.com/HaxeCheckstyle/tokentree/issues/53)
- Fixed `Binop(OpSub)` detection  [#54](https://github.com/HaxeCheckstyle/tokentree/issues/54)
- Fixed `final` handling in `FieldUtils` when compiling with Haxe 3 [#56](https://github.com/HaxeCheckstyle/tokentree/issues/56)
- Fixed handling of const type parameters [#57](https://github.com/HaxeCheckstyle/tokentree/issues/57)
- Fixed `case var` handling [#58](https://github.com/HaxeCheckstyle/tokentree/issues/58)
- Fixed `case Pattern(var foo, var bar)` handling [#59](https://github.com/HaxeCheckstyle/tokentree/issues/59)
- Fixed comments in if…else [#60](https://github.com/HaxeCheckstyle/tokentree/issues/60) +[#72](https://github.com/HaxeCheckstyle/tokentree/issues/72)
- Fixed comments in typedefs [#62](https://github.com/HaxeCheckstyle/tokentree/issues/62)
- Fixed handling of array items [#62](https://github.com/HaxeCheckstyle/tokentree/issues/62)
- Fixed handling of `null`, `true` and `false` as body of if [#63](https://github.com/HaxeCheckstyle/tokentree/issues/63)
- Changed `getPos` to calculate full position of all childs [#29](https://github.com/HaxeCheckstyle/tokentree/issues/29)
- Changed position of `enum` in `enum abstract` [#65](https://github.com/HaxeCheckstyle/tokentree/issues/65)
- Refactored `tempStore` handling and moved it to `TokenStream` childs [#28](https://github.com/HaxeCheckstyle/tokentree/issues/28) + [#30](https://github.com/HaxeCheckstyle/tokentree/issues/30)
- Refactored `TokenTreeAccessHelper` and made it an abstract [#31](https://github.com/HaxeCheckstyle/tokentree/issues/31) + [#32](https://github.com/HaxeCheckstyle/tokentree/issues/32) + [#41](https://github.com/HaxeCheckstyle/tokentree/issues/41)
- Refactored `isTypeEnumAbstract` [#33](https://github.com/HaxeCheckstyle/tokentree/issues/33)
- Refactored to apply formatting [#63](https://github.com/HaxeCheckstyle/tokentree/issues/63) +  [#65](https://github.com/HaxeCheckstyle/tokentree/issues/65)
- Refactored `filterCallback` to remove `Array.concat` calls [#66](https://github.com/HaxeCheckstyle/tokentree/issues/66)

## version 1.0.6 (2018-07-16)

- Added `TokenTreeCheckUtils.isTypeEnumAbstract` [#16](https://github.com/HaxeCheckstyle/tokentree/issues/16)
- Added FieldUtils [#17](https://github.com/HaxeCheckstyle/tokentree/issues/17)
- Added `isComment` and `isCIdent` to `TokentTreeAccessHelper` [#23](https://github.com/HaxeCheckstyle/tokentree/issues/23)
- Added `TokenTreeCheckUtils.isOpGtTypedefExtension` [#24](https://github.com/HaxeCheckstyle/tokentree/issues/24)
- Added `TokenTreeCheckUtils.isBrOpenAnonTypeOrTypedef` [#26](https://github.com/HaxeCheckstyle/tokentree/issues/26)
- Added `tempStore` to `TokenStream` [#27](https://github.com/HaxeCheckstyle/tokentree/issues/27)
- Fixed `@:default` [#18](https://github.com/HaxeCheckstyle/tokentree/issues/18)
- Fixed position of `cast` children [#19](https://github.com/HaxeCheckstyle/tokentree/issues/19)
- Fixed position of comments in `case`/`default` [#20](https://github.com/HaxeCheckstyle/tokentree/issues/20)
- Fixed error handling to work with `--no-inline` [#21](https://github.com/HaxeCheckstyle/tokentree/issues/21)
- Fixed position of BinOp() in anon objects [#22](https://github.com/HaxeCheckstyle/tokentree/issues/22)
- Fixed ternary handling [#25](https://github.com/HaxeCheckstyle/tokentree/issues/25)
- Fixed missing modifiers in conditionals [#27](https://github.com/HaxeCheckstyle/tokentree/issues/27)
- Changed `isImport` to also incluide `using` [#20](https://github.com/HaxeCheckstyle/tokentree/issues/20)

## version 1.0.5 (2018-07-08)

- Added support for `enum abstract` [#14](https://github.com/HaxeCheckstyle/tokentree/issues/14)
- Added support for `@:a.b.c` [#14](https://github.com/HaxeCheckstyle/tokentree/issues/14)
- Added support for `var ?x:Int` [#14](https://github.com/HaxeCheckstyle/tokentree/issues/14)
- Added support for `extern` fields [#14](https://github.com/HaxeCheckstyle/tokentree/issues/14)
- Changed position of Dot in method chains [#15](https://github.com/HaxeCheckstyle/tokentree/issues/15)

## version 1.0.4 (2018-06-30)

- Added parent and sibling access methods to TokenTreeAccessHelper [#10](https://github.com/HaxeCheckstyle/tokentree/issues/10)
- Added support for haxeparser's whitespace (`-D keep_whitespace`) [#12](https://github.com/HaxeCheckstyle/tokentree/issues/12)
- Fixed handling of `if`, `while` and `do`…`while` conditions [#11](https://github.com/HaxeCheckstyle/tokentree/issues/11)
- Fixed handling of ternary expressions [#11](https://github.com/HaxeCheckstyle/tokentree/issues/11)
- Fixed tree position of `Comma` [#13](https://github.com/HaxeCheckstyle/tokentree/issues/13)

## version 1.0.3 (2018-06-24)

- Fixed position of comments in abstracts and interfaces [#7](https://github.com/HaxeCheckstyle/tokentree/issues/7)
- Fixed unittest and coverage reporting for Haxe 4 [#8](https://github.com/HaxeCheckstyle/tokentree/issues/8)

## version 1.0.2 (2018-06-22)

- Fixed thread safety of SharpIf handling [#4](https://github.com/HaxeCheckstyle/tokentree/issues/4)
- Fixed handling of Semicolon in typedefs [#4](https://github.com/HaxeCheckstyle/tokentree/issues/4)
- Fixed WalkComment at end of stream [#4](https://github.com/HaxeCheckstyle/tokentree/issues/4)
- Fixed handling of switch in object declaration [#5](https://github.com/HaxeCheckstyle/tokentree/issues/5)
- Changed position of top level comments [#4](https://github.com/HaxeCheckstyle/tokentree/issues/4)

## version 1.0.1 (2018-06-14)

- Added `RELAX` mode to make parser more fault tolerant [#2](https://github.com/HaxeCheckstyle/tokentree/issues/2)
- Fixed handling of `cast -1` [#1](https://github.com/HaxeCheckstyle/tokentree/issues/1)

## version 1.0.0 (2018-06-09)

- initial move to separate repository
