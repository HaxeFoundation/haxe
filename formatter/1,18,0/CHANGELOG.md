# Version history

## dev branch / next version (1.x.x)

## version 1.18.0 (2024-12-23)

- added indentOffset to support code snippets with non-zero indentation levels
- fixed line break in type parameter constraint

## version 1.17.1 (2024-10-22)

- Fixed WhitespacePolicy compilation error

## version 1.17.0 (2024-10-06)

- **Breaking Change** replaced `whitespace.openingBracketPolicy` and `whitespace.closingBracketPolicy` settings with `whitespace.bracketConfig`, fixes [#592](https://github.com/HaxeCheckstyle/haxe-formatter/issues/592)
- Added wrap condition types `EqualItemLengths`, `AllItemLengthsLargerThan` and `AllItemLengthsLessThan`
- **Breaking Change** Fixed wrap condition type `AnyItemLengthLessThan` - use `AllItemLengthsLessThan` for old behaviour
- Fixed modifiers with complex conditionals, fixes [#332](https://github.com/HaxeCheckstyle/haxe-formatter/issues/332)
- Fixed unexpected array wrapping behavior, fixes [#340](https://github.com/HaxeCheckstyle/haxe-formatter/issues/340)
- Fixed conditionalized class declaration with constraints, fixes [#431](https://github.com/HaxeCheckstyle/haxe-formatter/issues/431)
- Fixed empty classes with conditional metadata
- Fixed class fields with conditional function signatures
- Fixed abstract enum abstracts with conditionals
- Fixed map wrapping rules to avoid fill line wrapping
- Fixed curly with comments, fixes [#445](https://github.com/HaxeCheckstyle/haxe-formatter/issues/445)
- Fixed array literals in ternarys
- Fixed expression try…catch, fixes [#509](https://github.com/HaxeCheckstyle/haxe-formatter/issues/509)
- Fixed long array access wrapping
- Fixed expression try…catch for allman curlies
- Fixed conditionalized chained function call, fixes [#375](https://github.com/HaxeCheckstyle/haxe-formatter/issues/375)
- Fixed sameLine.functionBody with metadata before body, fixes [#681](https://github.com/HaxeCheckstyle/haxe-formatter/issues/681)
- Fixed nested conditional types

## version 1.16.1 (2024-09-18)

- Refactored unittests to work with utest library ([#679](https://github.com/HaxeCheckstyle/haxe-formatter/issues/679))
- Removed munit library ([#679](https://github.com/HaxeCheckstyle/haxe-formatter/issues/679))
- Fixed parse error when trying to format conditional arrow function

## version 1.16.0 (2024-02-10)

- Added `wrapping.mapWrap`, fixes [#675](https://github.com/HaxeCheckstyle/haxe-formatter/issues/675) ([#677](https://github.com/HaxeCheckstyle/haxe-formatter/issues/677))
- Fixed comment indentation in empty switch cases, fixes [#392](https://github.com/HaxeCheckstyle/haxe-formatter/issues/392), [#174](https://github.com/HaxeCheckstyle/haxe-formatter/issues/174) ([#677](https://github.com/HaxeCheckstyle/haxe-formatter/issues/677))
- Fixed multiline comment indentation with conditionals, fixes [#499](https://github.com/HaxeCheckstyle/haxe-formatter/issues/499) ([#677](https://github.com/HaxeCheckstyle/haxe-formatter/issues/677))
- Fixed empty lines between static functions in abstract, fixes [#356](https://github.com/HaxeCheckstyle/haxe-formatter/issues/356) ([#677](https://github.com/HaxeCheckstyle/haxe-formatter/issues/677))
- Fixed multiline string literals with empty lines, fixes [#368](https://github.com/HaxeCheckstyle/haxe-formatter/issues/368) ([#677](https://github.com/HaxeCheckstyle/haxe-formatter/issues/677))
- Fixed expressionIf for array comprehension, fixes [#365](https://github.com/HaxeCheckstyle/haxe-formatter/issues/365) ([#677](https://github.com/HaxeCheckstyle/haxe-formatter/issues/677))

## version 1.15.0 (2023-09-05)

- Added `sameLine.ifElseSemicolonNextLine` to allow breaking `if (true) foo; else foo;`, fixes [#612](https://github.com/HaxeCheckstyle/haxe-formatter/issues/612) ([#668](https://github.com/HaxeCheckstyle/haxe-formatter/issues/668))
- Fixed whitespace before null safety operator
- Fixed keeping same line for `macro if` expressions
- Fixed wrapping with maxLineLength off by one, fixes [#670](https://github.com/HaxeCheckstyle/haxe-formatter/issues/670) ([#671](https://github.com/HaxeCheckstyle/haxe-formatter/issues/671))
- Fixed extends wrapping for interfaces, fixes [#669](https://github.com/HaxeCheckstyle/haxe-formatter/issues/669)
- Fixed empty lines between fields of enum abstract, fixes [#672](https://github.com/HaxeCheckstyle/haxe-formatter/issues/672) ([#673](https://github.com/HaxeCheckstyle/haxe-formatter/issues/673))
- Fixed empty lines for if with comment, fixes [#556](https://github. ([#673](https://github.com/HaxeCheckstyle/haxe-formatter/issues/673))com/HaxeCheckstyle/haxe-formatter/issues/556)
- Fixed empty lines for block level doc comments, fixes [#511](https://github.com/HaxeCheckstyle/haxe-formatter/issues/51) ([#673](https://github.com/HaxeCheckstyle/haxe-formatter/issues/673))

## version 1.14.6 (2023-02-22)

- Fixed inline call() and inline new issue ([#667](https://github.com/HaxeCheckstyle/haxe-formatter/issues/667))

## version 1.14.5 (2023-02-15)

- Fixed null pointer issues ([#666](https://github.com/HaxeCheckstyle/haxe-formatter/issues/666))

## version 1.14.4 (2022-12-14)

- Refactored PosInfosMacro to limit number of invocations of inner loop
- Fixed anon objects with metadata, fixes [#607](https://github.com/HaxeCheckstyle/haxe-formatter/issues/607) ([#662](https://github.com/HaxeCheckstyle/haxe-formatter/issues/662))
- Fixed ComplexType reification mixed with ternary, fixes [#638](https://github.com/HaxeCheckstyle/haxe-formatter/issues/638) ([#662](https://github.com/HaxeCheckstyle/haxe-formatter/issues/662))
- Fixed arrow function with optional parameter, fixes [#642](https://github.com/HaxeCheckstyle/haxe-formatter/issues/642) ([#662](https://github.com/HaxeCheckstyle/haxe-formatter/issues/662))
- Fixed comments in typedef definition, fixes [#643](https://github.com/HaxeCheckstyle/haxe-formatter/issues/643) ([#662](https://github.com/HaxeCheckstyle/haxe-formatter/issues/662))
- Fixed enum type parameter, fixes [#659](https://github.com/HaxeCheckstyle/haxe-formatter/issues/659) ([#662](https://github.com/HaxeCheckstyle/haxe-formatter/issues/662))
- Fixed macro type hint, fixes [#660](https://github.com/HaxeCheckstyle/haxe-formatter/issues/660) ([#662](https://github.com/HaxeCheckstyle/haxe-formatter/issues/662))

## version 1.14.3 (2022-09-21)

- Fixed whitespace issue in for loops when using parentheses around start value, fixes [vshaxe/vshaxe#545](https://github.com/vshaxe/vshaxe/issues/545) ([#658](https://github.com/HaxeCheckstyle/haxe-formatter/issues/658))

## version 1.14.2 (2022-09-14)

- Updated haxeparser to support latest Haxe nightly syntax ([#657](https://github.com/HaxeCheckstyle/haxe-formatter/issues/657))

## version 1.14.1 (2022-09-07)

- Improved support for inline markup ([#656](https://github.com/HaxeCheckstyle/haxe-formatter/issues/656))

## version 1.14.0 (2022-08-27)

- Added support for inline markup, fixes [#523](https://github.com/HaxeCheckstyle/haxe-formatter/issues/523) + [#624](https://github.com/HaxeCheckstyle/haxe-formatter/issues/624) ([#655](https://github.com/HaxeCheckstyle/haxe-formatter/issues/655))

## version 1.13.1 (2022-04-26)

- Fixed conditional line ends / wrapping ([#654](https://github.com/HaxeCheckstyle/haxe-formatter/issues/654))
- Fixed whitespace after from/to with arrow functions ([#654](https://github.com/HaxeCheckstyle/haxe-formatter/issues/654))

## version 1.13.0 (2022-03-18)

- Added default type parameter support, fixes [#650](https://github.com/HaxeCheckstyle/haxe-formatter/issues/650) ([#651](https://github.com/HaxeCheckstyle/haxe-formatter/issues/651))
- Added support for static locals ([#652](https://github.com/HaxeCheckstyle/haxe-formatter/issues/652))
- Fixed empty lines in anonymous functions, fixes [#644](https://github.com/HaxeCheckstyle/haxe-formatter/issues/644) ([#645](https://github.com/HaxeCheckstyle/haxe-formatter/issues/645))
- Fixed trailing whitespace getting moved to previous line, fixes [#646](https://github.com/HaxeCheckstyle/haxe-formatter/issues/646) ([#647](https://github.com/HaxeCheckstyle/haxe-formatter/issues/647))
- Fixed exclude handling ([#653](https://github.com/HaxeCheckstyle/haxe-formatter/issues/653))

## version 1.12.0 (2021-02-28)

- Added `lineEnds.lineEndCharacter` to set line end character used for output ([#633](https://github.com/HaxeCheckstyle/haxe-formatter/issues/633))
- Fixed support for overload access modifier, fixes [#626](https://github.com/HaxeCheckstyle/haxe-formatter/issues/626) ([#627](https://github.com/HaxeCheckstyle/haxe-formatter/issues/627))
- Fixed parens after curly block, fixes [#629](https://github.com/HaxeCheckstyle/haxe-formatter/issues/629) ([#631](https://github.com/HaxeCheckstyle/haxe-formatter/issues/631))
- Fixed local metadata linebreak, fixes [#630](https://github.com/HaxeCheckstyle/haxe-formatter/issues/630) ([#631](https://github.com/HaxeCheckstyle/haxe-formatter/issues/631) + [#636](https://github.com/HaxeCheckstyle/haxe-formatter/issues/636))
- Fixed `is as` formatted as `isas`, fixes [#634](https://github.com/HaxeCheckstyle/haxe-formatter/issues/634) ([#635](https://github.com/HaxeCheckstyle/haxe-formatter/issues/635))
- Retired Haxe 3.4.7 compile support ([#627](https://github.com/HaxeCheckstyle/haxe-formatter/issues/627))

## version 1.11.2 (2020-11-04)

- Fixed array type parameter, fixes [#622](https://github.com/HaxeCheckstyle/haxe-formatter/issues/622) ([#623](https://github.com/HaxeCheckstyle/haxe-formatter/issues/623))

## version 1.11.1 (2020-11-03)

- Fixed whitespace for negative const values ([#600](https://github.com/HaxeCheckstyle/haxe-formatter/issues/600))
- Fixed multiline comments with vars, fixes [#598](https://github.com/HaxeCheckstyle/haxe-formatter/issues/598) ([#600](https://github.com/HaxeCheckstyle/haxe-formatter/issues/600))
- Fixed whitespace for null safe navigation operator, fixes [#599](https://github.com/HaxeCheckstyle/haxe-formatter/issues/599) ([#600](https://github.com/HaxeCheckstyle/haxe-formatter/issues/600))
- Fixed sameline handling of metadata after `BrOpen` and `Semicolon`, fixes [#602](https://github.com/HaxeCheckstyle/haxe-formatter/issues/602) ([#603](https://github.com/HaxeCheckstyle/haxe-formatter/issues/603))
- Fixed whitespace of macro reification in call, fixes [#591](https://github.com/HaxeCheckstyle/haxe-formatter/issues/591) ([#603](https://github.com/HaxeCheckstyle/haxe-formatter/issues/603))
- Fixed emptyline after final in enum abstract, fixes [#601](https://github.com/HaxeCheckstyle/haxe-formatter/issues/601) ([#603](https://github.com/HaxeCheckstyle/haxe-formatter/issues/603))
- Fixed keep trailing whitespace of multiline comments, fixes [#593](https://github.com/HaxeCheckstyle/haxe-formatter/issues/593) ([#603](https://github.com/HaxeCheckstyle/haxe-formatter/issues/603))
- Fixed whitespace around is operator, fixes [#605](https://github.com/HaxeCheckstyle/haxe-formatter/issues/605) ([#606](https://github.com/HaxeCheckstyle/haxe-formatter/issues/606))
- Fixed newline before semicolon when used with binops, fixes [#614](https://github.com/HaxeCheckstyle/haxe-formatter/issues/614) ([#616](https://github.com/HaxeCheckstyle/haxe-formatter/issues/616))
- Fixed whitespace between `&&=` and `||=`, fixes [#615](https://github.com/HaxeCheckstyle/haxe-formatter/issues/615) ([#616](https://github.com/HaxeCheckstyle/haxe-formatter/issues/616))
- Fixed handling of missing semicolon, fixes [#410](https://github.com/HaxeCheckstyle/haxe-formatter/issues/410) ([#619](https://github.com/HaxeCheckstyle/haxe-formatter/issues/619))
- Fixed line breaks for anonymous type hints ([#619](https://github.com/HaxeCheckstyle/haxe-formatter/issues/619))
- Fixed indentation of wrapped expressions, fixes [#372](https://github.com/HaxeCheckstyle/haxe-formatter/issues/372) + [#452](https://github.com/HaxeCheckstyle/haxe-formatter/issues/452) + [#557](https://github.com/HaxeCheckstyle/haxe-formatter/issues/557) + [#613](https://github.com/HaxeCheckstyle/haxe-formatter/issues/613) + [#617](https://github.com/HaxeCheckstyle/haxe-formatter/issues/617) + [#618](https://github.com/HaxeCheckstyle/haxe-formatter/issues/618) ([#619](https://github.com/HaxeCheckstyle/haxe-formatter/issues/619))
- Refactored for tokentree API change ([#604](https://github.com/HaxeCheckstyle/haxe-formatter/issues/604) + [#609](https://github.com/HaxeCheckstyle/haxe-formatter/issues/609))
- Fixed indentation for block init of var with function, fixes [#611](https://github.com/HaxeCheckstyle/haxe-formatter/issues/611) ([#619](https://github.com/HaxeCheckstyle/haxe-formatter/issues/619))
- Fixed wrapping with multiline comment in parameters, fixes [#610](https://github.com/HaxeCheckstyle/haxe-formatter/issues/610) ([#619](https://github.com/HaxeCheckstyle/haxe-formatter/issues/619))
- Fixed type parameter exception ([#620](https://github.com/HaxeCheckstyle/haxe-formatter/issues/620))
- Fixed map initialisation with comment ([#620](https://github.com/HaxeCheckstyle/haxe-formatter/issues/620))
- Fixed Dollar chains ([#620](https://github.com/HaxeCheckstyle/haxe-formatter/issues/620))
- Refactored for deprecation of `is` ([#608](https://github.com/HaxeCheckstyle/haxe-formatter/issues/608))
- Refactored anonymous type wrapping ([#619](https://github.com/HaxeCheckstyle/haxe-formatter/issues/619))
- Refactored wrapping diagnostics to provide more info in logs ([#619](https://github.com/HaxeCheckstyle/haxe-formatter/issues/619))

## version 1.11.0 (2020-06-07)

- Added whitespace policies for `ifConditionParens`, `switchConditionParens`, `whileConditionParens`, `sharpConditionParens` and `catchParens` - all inside `whitespace.parenConfig`, fixes [#583](https://github.com/HaxeCheckstyle/haxe-formatter/issues/583) ([#585](https://github.com/HaxeCheckstyle/haxe-formatter/issues/585))
- Fixed type hint whitespace in anonymous types, fixes [#586](https://github.com/HaxeCheckstyle/haxe-formatter/issues/586) ([#587](https://github.com/HaxeCheckstyle/haxe-formatter/issues/587))
- Fixed type hint whitespace with conditionals, fixes [#582](https://github.com/HaxeCheckstyle/haxe-formatter/issues/582) ([#587](https://github.com/HaxeCheckstyle/haxe-formatter/issues/587))
- Fixed whitspace handling for anon types as type parameter, fixes [#588](https://github.com/HaxeCheckstyle/haxe-formatter/issues/588) ([#590](https://github.com/HaxeCheckstyle/haxe-formatter/issues/590))
- Fixed whitespace around @in, fixes [#594](https://github.com/HaxeCheckstyle/haxe-formatter/issues/594) ([#596](https://github.com/HaxeCheckstyle/haxe-formatter/issues/596))

## version 1.10.2 (2020-04-18)

- Fixed a null pointer exception in tokentree ([#584](https://github.com/HaxeCheckstyle/haxe-formatter/issues/584))

## version 1.10.1 (2020-04-12)

- Fixed broken indentation after case with OpOr pattern, fixes [#576](https://github.com/HaxeCheckstyle/haxe-formatter/issues/576) ([#581](https://github.com/HaxeCheckstyle/haxe-formatter/issues/581))
- Fixed missing linebreak between metadata and doc comment, fixes [#578](https://github.com/HaxeCheckstyle/haxe-formatter/issues/578) ([#581](https://github.com/HaxeCheckstyle/haxe-formatter/issues/581))
- Fixed indentation in anon function body, fixes [#577](https://github.com/HaxeCheckstyle/haxe-formatter/issues/577) ([#581](https://github.com/HaxeCheckstyle/haxe-formatter/issues/581))
- Changed default wrapping location of `casePattern` to `afterLast`, fixes [#579](https://github.com/HaxeCheckstyle/haxe-formatter/issues/579) ([#581](https://github.com/HaxeCheckstyle/haxe-formatter/issues/581))

## version 1.10.0 (2020-04-11)

- Added `lineEnds.anonFunctionCurly`, fixes [#538](https://github.com/HaxeCheckstyle/haxe-formatter/issues/538) ([#549](https://github.com/HaxeCheckstyle/haxe-formatter/issues/549))
- Added detection for and skipping files with merge conflicts, fixes [#558](https://github.com/HaxeCheckstyle/haxe-formatter/issues/558) ([#559](https://github.com/HaxeCheckstyle/haxe-formatter/issues/559))
- Added case pattern wrapping with `wrapping.casePattern`, fixes [#359](https://github.com/HaxeCheckstyle/haxe-formatter/issues/359) ([#566](https://github.com/HaxeCheckstyle/haxe-formatter/issues/566))
- Added support for `var @:meta name` ([#568](https://github.com/HaxeCheckstyle/haxe-formatter/issues/568))
- Added new wrapping rule condition type `exceedsMaxLineLength`, fixes [#572](https://github.com/HaxeCheckstyle/haxe-formatter/issues/572) ([#573](https://github.com/HaxeCheckstyle/haxe-formatter/issues/573) + [#574](https://github.com/HaxeCheckstyle/haxe-formatter/issues/574))
- Fixed indentation of wrapped callbacks, fixes [#470](https://github.com/HaxeCheckstyle/haxe-formatter/issues/470), [#476](https://github.com/HaxeCheckstyle/haxe-formatter/issues/476), [#540](https://github.com/HaxeCheckstyle/haxe-formatter/issues/540), [#546](https://github.com/HaxeCheckstyle/haxe-formatter/issues/546) ([#548](https://github.com/HaxeCheckstyle/haxe-formatter/issues/548))
- Fixed indentation of array literals in calls, fixes [#376](https://github.com/HaxeCheckstyle/haxe-formatter/issues/367) ([#548](https://github.com/HaxeCheckstyle/haxe-formatter/issues/548))
- Fixed indentation of array literals in assignments, fixes [#512](https://github.com/HaxeCheckstyle/haxe-formatter/issues/512) ([#548](https://github.com/HaxeCheckstyle/haxe-formatter/issues/548))
- Fixed indentation of object literals, fixes [#490](https://github.com/HaxeCheckstyle/haxe-formatter/issues/490) ([#548](https://github.com/HaxeCheckstyle/haxe-formatter/issues/548))
- Fixed handling of unary OpSub, fixes [#547](https://github.com/HaxeCheckstyle/haxe-formatter/issues/547) ([#548](https://github.com/HaxeCheckstyle/haxe-formatter/issues/548))
- Fixed `expressionIf` detection in switch, fixes [#552](https://github.com/HaxeCheckstyle/haxe-formatter/issues/552) ([#553](https://github.com/HaxeCheckstyle/haxe-formatter/issues/553))
- Fixed keep wrapping with multiline strings, fixes [#561](https://github.com/HaxeCheckstyle/haxe-formatter/issues/561) ([#564](https://github.com/HaxeCheckstyle/haxe-formatter/issues/564))
- Fixed whitespace of block comment in array literals, fixes [#441](https://github.com/HaxeCheckstyle/haxe-formatter/issues/441) ([#564](https://github.com/HaxeCheckstyle/haxe-formatter/issues/564))
- Fixed whitespace of conditionalised case ([#564](https://github.com/HaxeCheckstyle/haxe-formatter/issues/564))
- Fixed conditionalised class declaration with constraints, fixes [#431](https://github.com/HaxeCheckstyle/haxe-formatter/issues/431) ([#564](https://github.com/HaxeCheckstyle/haxe-formatter/issues/564))
- Fixed conditionalised function type typedef, fixes [#531](https://github.com/HaxeCheckstyle/haxe-formatter/issues/531) ([#564](https://github.com/HaxeCheckstyle/haxe-formatter/issues/564))
- Fixed arrow function in map literal, fixes [#426](https://github.com/HaxeCheckstyle/haxe-formatter/issues/426) ([#564](https://github.com/HaxeCheckstyle/haxe-formatter/issues/564))
- Fixed whitespace in single argument arrow function, fixes [#562](https://github.com/HaxeCheckstyle/haxe-formatter/issues/562) ([#564](https://github.com/HaxeCheckstyle/haxe-formatter/issues/564))
- Fixed whitespace of final in interfaces, fixes [#563](https://github.com/HaxeCheckstyle/haxe-formatter/issues/563) ([#564](https://github.com/HaxeCheckstyle/haxe-formatter/issues/564))
- Fixed newline after macro class, fixes [#565](https://github.com/HaxeCheckstyle/haxe-formatter/issues/565) ([#566](https://github.com/HaxeCheckstyle/haxe-formatter/issues/566))
- Fixed indentation of if-expression with metadata, fixes [#567](https://github.com/HaxeCheckstyle/haxe-formatter/issues/567) ([#568](https://github.com/HaxeCheckstyle/haxe-formatter/issues/568))
- Fixed wrapping of object literals with OpBoolOr/OpBooland, fixes [#569](https://github.com/HaxeCheckstyle/haxe-formatter/issues/569) ([#570](https://github.com/HaxeCheckstyle/haxe-formatter/issues/570))
- Fixed handling of parens in `@:default(1)` ([#570](https://github.com/HaxeCheckstyle/haxe-formatter/issues/570))
- Fixed range format when endpos is inside token, fixes [vshaxe/vshaxe#425](https://github.com/vshaxe/vshaxe/issues/425) ([#570](https://github.com/HaxeCheckstyle/haxe-formatter/issues/570))
- Changed keep-like behaviour of `sameLine.expressionIf` with `same`, fixes [#304](https://github.com/HaxeCheckstyle/haxe-formatter/issues/304) ([#548](https://github.com/HaxeCheckstyle/haxe-formatter/issues/548) + [#550](https://github.com/HaxeCheckstyle/haxe-formatter/issues/550))

## version 1.9.2 (2019-12-19)

- Added unittests for empty lines ([#535](https://github.com/HaxeCheckstyle/haxe-formatter/issues/535))
- Fixed range format issues with multiline tokens ([#532](https://github.com/HaxeCheckstyle/haxe-formatter/issues/532) + [#533](https://github.com/HaxeCheckstyle/haxe-formatter/issues/533))
- Fixed extra indentation for parens after `=`, fixes [#534](https://github.com/HaxeCheckstyle/haxe-formatter/issues/534) ([#535](https://github.com/HaxeCheckstyle/haxe-formatter/issues/535))
- Fixed method chain with comments detection and wrapping ([#536](https://github.com/HaxeCheckstyle/haxe-formatter/issues/536))
- Fixed whitespace before metadata ([#537](https://github.com/HaxeCheckstyle/haxe-formatter/issues/537))
- Fixed whitespace after else with ifPolicy, fixes [#543](https://github.com/HaxeCheckstyle/haxe-formatter/issues/543)  ([#544](https://github.com/HaxeCheckstyle/haxe-formatter/issues/544))
- Fixed emptyline afterReturn when return is body of function ([#544](https://github.com/HaxeCheckstyle/haxe-formatter/issues/544))
- Refactored build system to use lix ([#537](https://github.com/HaxeCheckstyle/haxe-formatter/issues/537) + [#539](https://github.com/HaxeCheckstyle/haxe-formatter/issues/539))

## version 1.9.1 (2019-09-12)

- Fixed range format issues ([#528](https://github.com/HaxeCheckstyle/haxe-formatter/issues/528) + [#529](https://github.com/HaxeCheckstyle/haxe-formatter/issues/529) + [#530](https://github.com/HaxeCheckstyle/haxe-formatter/issues/530))

## version 1.9.0 (2019-09-10)

- Added `fixedZeroIncrease` and `fixedZeroIncreaseBlocks` policies to `indentation.conditionalPolicy` ([#514](https://github.com/HaxeCheckstyle/haxe-formatter/issues/514))
- Added `alignedNestedIncrease` policies to `indentation.conditionalPolicy`, fixes [#519](https://github.com/HaxeCheckstyle/haxe-formatter/issues/519) ([#520](https://github.com/HaxeCheckstyle/haxe-formatter/issues/520))
- Added range to format API ([#524](https://github.com/HaxeCheckstyle/haxe-formatter/issues/524))
- Fixed missing space when returning an arrow function, fixes [#513](https://github.com/HaxeCheckstyle/haxe-formatter/issues/513) ([#514](https://github.com/HaxeCheckstyle/haxe-formatter/issues/514))
- Fixed conditionals in return types, fixes [#446](https://github.com/HaxeCheckstyle/haxe-formatter/issues/446) ([#515](https://github.com/HaxeCheckstyle/haxe-formatter/issues/515))
- Fixed null pointer issues in MarkWhitespace ([#521](https://github.com/HaxeCheckstyle/haxe-formatter/issues/521))
- Fixed path handling in Hashlink 1.1 ([#521](https://github.com/HaxeCheckstyle/haxe-formatter/issues/521))
- Fixed postfix exclamation mark, fixes [#516](https://github.com/HaxeCheckstyle/haxe-formatter/issues/516) ([#527](https://github.com/HaxeCheckstyle/haxe-formatter/issues/527))

## version 1.8.1 (2019-06-27)

- Fixed debug info in command line version ([#517](https://github.com/HaxeCheckstyle/haxe-formatter/issues/517))
- Fixed total runtime calculation ([#517](https://github.com/HaxeCheckstyle/haxe-formatter/issues/517))

## version 1.8.0 (2019-06-13)

- Added `sameLine.returnBodySingleLine`, fixes [#303](https://github.com/HaxeCheckstyle/haxe-formatter/issues/303) ([#483](https://github.com/HaxeCheckstyle/haxe-formatter/issues/483))
- Added `emptyLines.*.existingBetweenFields`, fixes [#455](https://github.com/HaxeCheckstyle/haxe-formatter/issues/455) ([#484](https://github.com/HaxeCheckstyle/haxe-formatter/issues/484))
- Added printing config filename in verbose mode, fixes [#460](https://github.com/HaxeCheckstyle/haxe-formatter/issues/460) ([#493](https://github.com/HaxeCheckstyle/haxe-formatter/issues/493))
- Added `indentation.indentCaseLabels`, fixes [#478](https://github.com/HaxeCheckstyle/haxe-formatter/issues/478) ([#502](https://github.com/HaxeCheckstyle/haxe-formatter/issues/502))
- Added `sameLine.untypedBody`, fixes [#362](https://github.com/HaxeCheckstyle/haxe-formatter/issues/362) ([#506](https://github.com/HaxeCheckstyle/haxe-formatter/issues/506))
- Fixed same line handling of if-else with try catch body, fixes [#360](https://github.com/HaxeCheckstyle/haxe-formatter/issues/360) ([#483](https://github.com/HaxeCheckstyle/haxe-formatter/issues/483))
- Fixed line end handling of structure type as type parameter, fixes [#475](https://github.com/HaxeCheckstyle/haxe-formatter/issues/475) ([#486](https://github.com/HaxeCheckstyle/haxe-formatter/issues/486))
- Fixed wrapping function parameters with comments, fixes [#472](https://github.com/HaxeCheckstyle/haxe-formatter/issues/472) ([#487](https://github.com/HaxeCheckstyle/haxe-formatter/issues/487))
- Fixed line break right before, fixes [#311](https://github.com/HaxeCheckstyle/haxe-formatter/issues/311) ([#456](https://github.com/HaxeCheckstyle/haxe-formatter/issues/456))
- Fixed wrapping of array comprehension, fixes [#357](https://github.com/HaxeCheckstyle/haxe-formatter/issues/357) + [#366](https://github.com/HaxeCheckstyle/haxe-formatter/issues/366) ([#491](https://github.com/HaxeCheckstyle/haxe-formatter/issues/491))
- Fixed type hint detection for enums and overload metas, fixes [#488](https://github.com/HaxeCheckstyle/haxe-formatter/issues/488) ([#491](https://github.com/HaxeCheckstyle/haxe-formatter/issues/491))
- Fixed comments in OpAdd chains and case, fixes [#396](https://github.com/HaxeCheckstyle/haxe-formatter/issues/396) + [#477](https://github.com/HaxeCheckstyle/haxe-formatter/issues/477) ([#492](https://github.com/HaxeCheckstyle/haxe-formatter/issues/492))
- Fixed wrapping of call, fixes [#386](https://github.com/HaxeCheckstyle/haxe-formatter/issues/386) ([#492](https://github.com/HaxeCheckstyle/haxe-formatter/issues/492))
- Fixed nested method chain handling, fixes [#496](https://github.com/HaxeCheckstyle/haxe-formatter/issues/496) ([#497](https://github.com/HaxeCheckstyle/haxe-formatter/issues/497))
- Fixed nested inline array comprehension, fixes [#498](https://github.com/HaxeCheckstyle/haxe-formatter/issues/498) ([#501](https://github.com/HaxeCheckstyle/haxe-formatter/issues/501))
- Fixed incorrect comment closing, fixes [#500](https://github.com/HaxeCheckstyle/haxe-formatter/issues/500) ([#501](https://github.com/HaxeCheckstyle/haxe-formatter/issues/501))
- Fixed imports with conditionals, fixes [#504](https://github.com/HaxeCheckstyle/haxe-formatter/issues/504) ([#505](https://github.com/HaxeCheckstyle/haxe-formatter/issues/505))
- Fixed line break in type parameters, fixes [#494](https://github.com/HaxeCheckstyle/haxe-formatter/issues/494) ([#507](https://github.com/HaxeCheckstyle/haxe-formatter/issues/507))
- Changed `sameLine.returnBody` to only apply to multiline or loop/switch/try/if expressions, fixes [#303](https://github.com/HaxeCheckstyle/haxe-formatter/issues/303) ([#483](https://github.com/HaxeCheckstyle/haxe-formatter/issues/483))

## version 1.7.1 (2019-06-01)

- Fixed indentation of nested object literals ([#479](https://github.com/HaxeCheckstyle/haxe-formatter/issues/479))
- Fixed array wrapping in call parameters, fixes [#466](https://github.com/HaxeCheckstyle/haxe-formatter/issues/466) ([#479](https://github.com/HaxeCheckstyle/haxe-formatter/issues/479))
- Fixed conditional using dot ident without parens, fixes [#480](https://github.com/HaxeCheckstyle/haxe-formatter/issues/480) ([#481](https://github.com/HaxeCheckstyle/haxe-formatter/issues/481))

## version 1.7.0 (2019-05-17)

- Added `wrapping.multiVar`, fixes [#355](https://github.com/HaxeCheckstyle/haxe-formatter/issues/355), fixes [#430](https://github.com/HaxeCheckstyle/haxe-formatter/issues/430) ([#422](https://github.com/HaxeCheckstyle/haxe-formatter/issues/422) + [#434](https://github.com/HaxeCheckstyle/haxe-formatter/issues/434))
- Added `emptylines.afterFieldsWithDocComments`, fixes [#385](https://github.com/HaxeCheckstyle/haxe-formatter/issues/385), fixes [#432](https://github.com/HaxeCheckstyle/haxe-formatter/issues/432) ([#425](https://github.com/HaxeCheckstyle/haxe-formatter/issues/425) + [#434](https://github.com/HaxeCheckstyle/haxe-formatter/issues/434))
- Added `lineEnds.anonTypeCurly`, `lineEnds.blockCurly`, `lineEnds.objectLiteralCurly`, `lineEnds.typedefCurly`, fixes [#346](https://github.com/HaxeCheckstyle/haxe-formatter/issues/346) ([#427](https://github.com/HaxeCheckstyle/haxe-formatter/issues/427) + [#434](https://github.com/HaxeCheckstyle/haxe-formatter/issues/434) + [#456](https://github.com/HaxeCheckstyle/haxe-formatter/issues/456))
- Added `wrapping.arrayMatrixWrap` for array matrix wrapping with column alignment, fixes [#433](https://github.com/HaxeCheckstyle/haxe-formatter/issues/433) ([#442](https://github.com/HaxeCheckstyle/haxe-formatter/issues/442))
- Added Java compilation and tests on TravisCI ([#456](https://github.com/HaxeCheckstyle/haxe-formatter/issues/456))
- Added browser JS compilation, fixes [#449](https://github.com/HaxeCheckstyle/haxe-formatter/issues/449) ([#456](https://github.com/HaxeCheckstyle/haxe-formatter/issues/456))
- Added cache for close tokens `]`, `)` and `}` ([#461](https://github.com/HaxeCheckstyle/haxe-formatter/issues/461))
- Added `indentation.indentComplexValueExpressions`, fixes [#468](https://github.com/HaxeCheckstyle/haxe-formatter/issues/468) ([#469](https://github.com/HaxeCheckstyle/haxe-formatter/issues/469))
- Fixed missing empty lines in classes with conditionals, fixes [#419](https://github.com/HaxeCheckstyle/haxe-formatter/issues/419) ([#422](https://github.com/HaxeCheckstyle/haxe-formatter/issues/422))
- Fixed wrapping of concatenated strings ([#422](https://github.com/HaxeCheckstyle/haxe-formatter/issues/422)
- Fixed ECheckType detection with cast, fixes [#374](https://github.com/HaxeCheckstyle/haxe-formatter/issues/374) ([#422](https://github.com/HaxeCheckstyle/haxe-formatter/issues/422))
- Fixed same line handling of cases with object pattern, fixes [#306](https://github.com/HaxeCheckstyle/haxe-formatter/issues/306) ([#422](https://github.com/HaxeCheckstyle/haxe-formatter/issues/422))
- Fixed ETypeCheck with conditional, fixes [#395](https://github.com/HaxeCheckstyle/haxe-formatter/issues/395) ([#422](https://github.com/HaxeCheckstyle/haxe-formatter/issues/422))
- Fixed wrong OpBool indentation in nested switch/case, fixes [#423](https://github.com/HaxeCheckstyle/haxe-formatter/issues/423) ([#424](https://github.com/HaxeCheckstyle/haxe-formatter/issues/424))
- Fixed `afterFieldsWithDocComments` with conditionals, fixes [#428](https://github.com/HaxeCheckstyle/haxe-formatter/issues/428)
- Fixed default wrapping stopping after first line break ([#436](https://github.com/HaxeCheckstyle/haxe-formatter/issues/436))
- Fixed wrapping in field access, fixes [#314](https://github.com/HaxeCheckstyle/haxe-formatter/issues/314) ([#442](https://github.com/HaxeCheckstyle/haxe-formatter/issues/442))
- Fixed object literal detection when using expression level entry point, fixes [#450](https://github.com/HaxeCheckstyle/haxe-formatter/issues/450) ([#451](https://github.com/HaxeCheckstyle/haxe-formatter/issues/451))
- Fixed null check for indentation on expression level entry points ([#453](https://github.com/HaxeCheckstyle/haxe-formatter/issues/453))
- Fixed length check in removeBOM ([#453](https://github.com/HaxeCheckstyle/haxe-formatter/issues/453))
- Fixed semicolon after `#end`, fixes [#429](https://github.com/HaxeCheckstyle/haxe-formatter/issues/429) ([#456](https://github.com/HaxeCheckstyle/haxe-formatter/issues/456))
- Fixed comments in array and matrix wrapping, fixes [#443](https://github.com/HaxeCheckstyle/haxe-formatter/issues/443) + [#444](https://github.com/HaxeCheckstyle/haxe-formatter/issues/444) ([#456](https://github.com/HaxeCheckstyle/haxe-formatter/issues/456))
- Fixed eof handling in stdin mode when running on Windows, fixes [#418](https://github.com/HaxeCheckstyle/haxe-formatter/issues/418) ([#457](https://github.com/HaxeCheckstyle/haxe-formatter/issues/457))
- Fixed indentation of if / else in complex var assignments, fixes [#334](https://github.com/HaxeCheckstyle/haxe-formatter/issues/334) ([#458](https://github.com/HaxeCheckstyle/haxe-formatter/issues/458))
- Fixed ECheckType in if body, fixes [#354](https://github.com/HaxeCheckstyle/haxe-formatter/issues/354) ([#459](https://github.com/HaxeCheckstyle/haxe-formatter/issues/459))
- Fixed whitespace between IntInterval and POpen ([#462](https://github.com/HaxeCheckstyle/haxe-formatter/issues/462))
- Fixed incorrect function header wrapping, fixes [#439](https://github.com/HaxeCheckstyle/haxe-formatter/issues/439) ([#463](https://github.com/HaxeCheckstyle/haxe-formatter/issues/463))
- Fixed incorrect wrapping of multiline strings, fixes [#438](https://github.com/HaxeCheckstyle/haxe-formatter/issues/438) ([#463](https://github.com/HaxeCheckstyle/haxe-formatter/issues/463))
- Fixed indentation of if in value place, fixes [#464](https://github.com/HaxeCheckstyle/haxe-formatter/issues/464) [#465](https://github.com/HaxeCheckstyle/haxe-formatter/issues/465) ([#467](https://github.com/HaxeCheckstyle/haxe-formatter/issues/467))
- Changed default value for `excludes` to include `node_modules` ([#420](https://github.com/HaxeCheckstyle/haxe-formatter/issues/420))
- Changed debug code to produce less unneccessary code in non debug mode ([#417](https://github.com/HaxeCheckstyle/haxe-formatter/issues/417))
- Refactored wrapping to use an ordered list ([#436](https://github.com/HaxeCheckstyle/haxe-formatter/issues/436))
- Refactored array wrapping ([#442](https://github.com/HaxeCheckstyle/haxe-formatter/issues/442))
- Updated testcases for [#305](https://github.com/HaxeCheckstyle/haxe-formatter/issues/305) ([#422](https://github.com/HaxeCheckstyle/haxe-formatter/issues/422))
- Removed compiletime library, fixes [#390](https://github.com/HaxeCheckstyle/haxe-formatter/issues/390) ([#467](https://github.com/HaxeCheckstyle/haxe-formatter/issues/467))

## version 1.6.0 (2019-03-22)

- Added `--stdin` CLI switch to allow reading code from stdin and printing formatted code to stdout, fixes [#411](https://github.com/HaxeCheckstyle/haxe-formatter/issues/411) ([#412](https://github.com/HaxeCheckstyle/haxe-formatter/issues/412) + [#413](https://github.com/HaxeCheckstyle/haxe-formatter/issues/413))
- Fixed specifiying an entry point for tokentree, fixes [#393](https://github.com/HaxeCheckstyle/haxe-formatter/issues/393) ([#403](https://github.com/HaxeCheckstyle/haxe-formatter/issues/403) + [#404](https://github.com/HaxeCheckstyle/haxe-formatter/issues/404))
- Fixed whitespace for conditionals in function arguments with question mark, fixes [#397](https://github.com/HaxeCheckstyle/haxe-formatter/issues/397) ([#405](https://github.com/HaxeCheckstyle/haxe-formatter/issues/405))
- Fixed unary OpSub in arrays ([#405](https://github.com/HaxeCheckstyle/haxe-formatter/issues/405))
- Fixed whitespace for else body with ECheckType, fixes [#361](https://github.com/HaxeCheckstyle/haxe-formatter/issues/361) ([#405](https://github.com/HaxeCheckstyle/haxe-formatter/issues/405))
- Fixed indentation after block breaking conditional, fixes [#290](https://github.com/HaxeCheckstyle/haxe-formatter/issues/290) ([#406](https://github.com/HaxeCheckstyle/haxe-formatter/issues/406) + [#408](https://github.com/HaxeCheckstyle/haxe-formatter/issues/408))
- Fixed sameline format of anon types with wrapping set to keep, fixes [#301](https://github.com/HaxeCheckstyle/haxe-formatter/issues/301) ([#412](https://github.com/HaxeCheckstyle/haxe-formatter/issues/412))
- Changed default value for `excludes` to include `.git` folders ([#399](https://github.com/HaxeCheckstyle/haxe-formatter/issues/399))
- Changed using comma as an indicator for simple wrapping ([#403](https://github.com/HaxeCheckstyle/haxe-formatter/issues/403))
- Changed formatter api calls, fixes [#393](https://github.com/HaxeCheckstyle/haxe-formatter/issues/393) ([#403](https://github.com/HaxeCheckstyle/haxe-formatter/issues/403))
- Changed command line help text, fixes [#400](https://github.com/HaxeCheckstyle/haxe-formatter/issues/400) ([#402](https://github.com/HaxeCheckstyle/haxe-formatter/issues/402))
- Replaced custom schema generator with json2objects ([#398](https://github.com/HaxeCheckstyle/haxe-formatter/issues/398))

## version 1.5.1 (2019-03-06)

- Fixed vanishing string literals at end of file ([#391](https://github.com/HaxeCheckstyle/haxe-formatter/issues/391))

## version 1.5.0 (2019-03-04)

- Added `wrapping.opAddSubChain` ([#370](https://github.com/HaxeCheckstyle/haxe-formatter/issues/370))
- Added `wrapping.metadataCallParameter` ([#370](https://github.com/HaxeCheckstyle/haxe-formatter/issues/370))
- Added `emptyLines.macroClassEmptyLines`, fixes [#377](https://github.com/HaxeCheckstyle/haxe-formatter/issues/377) ([#383](https://github.com/HaxeCheckstyle/haxe-formatter/issues/383))
- Added `emptyLines.lineCommentsBetweenTypes` and `emptyLines.lineCommentsBetweenFunctions` to separate line comments from types and functions ([#387](https://github.com/HaxeCheckstyle/haxe-formatter/issues/387) + [#389](https://github.com/HaxeCheckstyle/haxe-formatter/issues/389))
- Added `whitespace.addLineCommentSpace` to ensure a space after `//` ([#388](https://github.com/HaxeCheckstyle/haxe-formatter/issues/388))
- Fixed type parameter constraint with structure type, fixes [#337](https://github.com/HaxeCheckstyle/haxe-formatter/issues/337) ([#349](https://github.com/HaxeCheckstyle/haxe-formatter/issues/349))
- Fixed wrapping of OpBool chains with null ([#349](https://github.com/HaxeCheckstyle/haxe-formatter/issues/349))
- Fixed line comments after typedefs, fixes [#331](https://github.com/HaxeCheckstyle/haxe-formatter/issues/331) ([#349](https://github.com/HaxeCheckstyle/haxe-formatter/issues/349))
- Fixed line comments after #end, fixes [#344](https://github.com/HaxeCheckstyle/haxe-formatter/issues/344) ([#349](https://github.com/HaxeCheckstyle/haxe-formatter/issues/349))
- Fixed comments before catch, fixes [#339](https://github.com/HaxeCheckstyle/haxe-formatter/issues/339) ([#349](https://github.com/HaxeCheckstyle/haxe-formatter/issues/349))
- Fixed ternary with object literal, fixes [#330](https://github.com/HaxeCheckstyle/haxe-formatter/issues/330) ([#349](https://github.com/HaxeCheckstyle/haxe-formatter/issues/349))
- Fixed optional function parameters in conditionals, fixes [#345](https://github.com/HaxeCheckstyle/haxe-formatter/issues/345) ([#349](https://github.com/HaxeCheckstyle/haxe-formatter/issues/349))
- Fixed whitespace for ternary and unary operators, fixes [#348](https://github.com/HaxeCheckstyle/haxe-formatter/issues/348) ([#349](https://github.com/HaxeCheckstyle/haxe-formatter/issues/349))
- Fixed if body starting with metadata, fixes [#333](https://github.com/HaxeCheckstyle/haxe-formatter/issues/333) ([#350](https://github.com/HaxeCheckstyle/haxe-formatter/issues/350))
- Fixed else body starting with POpen, fixes [#329](https://github.com/HaxeCheckstyle/haxe-formatter/issues/329) ([#350](https://github.com/HaxeCheckstyle/haxe-formatter/issues/350))
- Fixed missing space after conditional, fixes [#327](https://github.com/HaxeCheckstyle/haxe-formatter/issues/327) + [#335](https://github.com/HaxeCheckstyle/haxe-formatter/issues/335) ([#350](https://github.com/HaxeCheckstyle/haxe-formatter/issues/350))
- Fixed braceless if body with conditionals, fixes [#328](https://github.com/HaxeCheckstyle/haxe-formatter/issues/328) ([#351](https://github.com/HaxeCheckstyle/haxe-formatter/issues/351))
- Fixed complex conditional typedef, fixes [#336](https://github.com/HaxeCheckstyle/haxe-formatter/issues/336) ([#351](https://github.com/HaxeCheckstyle/haxe-formatter/issues/351))
- Fixed parameter wrapping with multiline string, fixes [#320](https://github.com/HaxeCheckstyle/haxe-formatter/issues/320) ([#351](https://github.com/HaxeCheckstyle/haxe-formatter/issues/351))
- Fixed indentation of untyped assignment, fixes [#326](https://github.com/HaxeCheckstyle/haxe-formatter/issues/326) ([#352](https://github.com/HaxeCheckstyle/haxe-formatter/issues/352))
- Fixed indentation of anon function call, fixes [#315](https://github.com/HaxeCheckstyle/haxe-formatter/issues/315) ([#352](https://github.com/HaxeCheckstyle/haxe-formatter/issues/352))
- Fixed `keep` wrapping option to respect both `beforeLast` and `afterLast` wrapping location ([#370](https://github.com/HaxeCheckstyle/haxe-formatter/issues/370))
- Fixed abstract with comments, fixes [#363](https://github.com/HaxeCheckstyle/haxe-formatter/issues/363), fixes [#364](https://github.com/HaxeCheckstyle/haxe-formatter/issues/364) ([#370](https://github.com/HaxeCheckstyle/haxe-formatter/issues/370))
- Fixed handling of `@:using`, `@:import` and `@:package`, fixes [#378](https://github.com/HaxeCheckstyle/haxe-formatter/issues/378) ([#379](https://github.com/HaxeCheckstyle/haxe-formatter/issues/379))
- Fixed null pointer exception when handling incomplete metadata, fixes [#380](https://github.com/HaxeCheckstyle/haxe-formatter/issues/380) ([#381](https://github.com/HaxeCheckstyle/haxe-formatter/issues/381))
- Fixed left curly placement for anon types as return values, fixes [#358](https://github.com/HaxeCheckstyle/haxe-formatter/issues/358), fixes [#369](https://github.com/HaxeCheckstyle/haxe-formatter/issues/369) ([#387](https://github.com/HaxeCheckstyle/haxe-formatter/issues/387))
- Fixed empty lines after metadata in macro classes, fixes [#384](https://github.com/HaxeCheckstyle/haxe-formatter/issues/384) ([#387](https://github.com/HaxeCheckstyle/haxe-formatter/issues/387))
- Fixed line ends after opadd chain as function body, fixes [#376](https://github.com/HaxeCheckstyle/haxe-formatter/issues/376) ([#387](https://github.com/HaxeCheckstyle/haxe-formatter/issues/387))

## version 1.4.0 (2019-02-07)

- Added haxe-test-adapter [#286](https://github.com/HaxeCheckstyle/haxe-formatter/issues/286) + [#287](https://github.com/HaxeCheckstyle/haxe-formatter/issues/287) + [#289](https://github.com/HaxeCheckstyle/haxe-formatter/issues/289)
- Added `wrapping.implementsExtends` [#288](https://github.com/HaxeCheckstyle/haxe-formatter/issues/288)
- Added `emptylines.afterFileHeaderComment`and `emptylines.betweenMultilineComments` fixes [#292](https://github.com/HaxeCheckstyle/haxe-formatter/issues/292) ([#296](https://github.com/HaxeCheckstyle/haxe-formatter/issues/296))
- Added wrapping location to allow wrapping before delimiting token, fixes [#299](https://github.com/HaxeCheckstyle/haxe-formatter/issues/299) ([#309](https://github.com/HaxeCheckstyle/haxe-formatter/issues/309))
- Fixed whitespace of type check in array comprehension, fixes [#284](https://github.com/HaxeCheckstyle/haxe-formatter/issues/284) ([#285](https://github.com/HaxeCheckstyle/haxe-formatter/issues/285))
- Fixed conditional modifier handling, fixes [#291](https://github.com/HaxeCheckstyle/haxe-formatter/issues/291) ([#293](https://github.com/HaxeCheckstyle/haxe-formatter/issues/293))
- Fixed whitespace after left curly in anon type hints [#297](https://github.com/HaxeCheckstyle/haxe-formatter/issues/297)
- Fixed anon types handling when `leftCurly` is set to `both`, fixes [#301](https://github.com/HaxeCheckstyle/haxe-formatter/issues/301) ([#309](https://github.com/HaxeCheckstyle/haxe-formatter/issues/309))
- Fixed same line handling of `expressionIf`, fixes [#304](https://github.com/HaxeCheckstyle/haxe-formatter/issues/304) ([#309](https://github.com/HaxeCheckstyle/haxe-formatter/issues/309))
- Fixed indentation of object literals, fixes [#305](https://github.com/HaxeCheckstyle/haxe-formatter/issues/305) ([#309](https://github.com/HaxeCheckstyle/haxe-formatter/issues/309))
- Fixed same line handling of cases with object pattern, fixes [#306](https://github.com/HaxeCheckstyle/haxe-formatter/issues/306) ([#309](https://github.com/HaxeCheckstyle/haxe-formatter/issues/309))
- Fixed empty line after metadata with function, fixes [#307](https://github.com/HaxeCheckstyle/haxe-formatter/issues/307) ([#309](https://github.com/HaxeCheckstyle/haxe-formatter/issues/309))
- Fixed missing space before `(`, fixes [#308](https://github.com/HaxeCheckstyle/haxe-formatter/issues/308) ([#309](https://github.com/HaxeCheckstyle/haxe-formatter/issues/309))
- Fixed detection of file header vs. type doc comments, fixes [#292](https://github.com/HaxeCheckstyle/haxe-formatter/issues/292) ([#312](https://github.com/HaxeCheckstyle/haxe-formatter/issues/312))
- Fixed detection of file header vs. type doc comments, fixes [#316](https://github.com/HaxeCheckstyle/haxe-formatter/issues/316) ([#318](https://github.com/HaxeCheckstyle/haxe-formatter/issues/318))
- Fixed multiline comments after typedefs without semicolon, fixes [#322](https://github.com/HaxeCheckstyle/haxe-formatter/issues/321) ([#323](https://github.com/HaxeCheckstyle/haxe-formatter/issues/323))
- Fixed empty line between conditional imports and types, fixes [#322](https://github.com/HaxeCheckstyle/haxe-formatter/issues/322) ([#323](https://github.com/HaxeCheckstyle/haxe-formatter/issues/323))
- Fixed `lineEnds.leftCurly` both for object literals, fixes [#319](https://github.com/HaxeCheckstyle/haxe-formatter/issues/319) ([#325](https://github.com/HaxeCheckstyle/haxe-formatter/issues/325))
- Changed default rules for `wrapping.opBoolChain`, fixes [#313](https://github.com/HaxeCheckstyle/haxe-formatter/issues/313) ([#324](https://github.com/HaxeCheckstyle/haxe-formatter/issues/324))
- Refactored to add some null safety (incomplete) [#309](https://github.com/HaxeCheckstyle/haxe-formatter/issues/309)
- Removed stacktrace from output with invalid `hxformat.json` files, fixes [#300](https://github.com/HaxeCheckstyle/haxe-formatter/issues/300) ([#309](https://github.com/HaxeCheckstyle/haxe-formatter/issues/309))

## version 1.3.0 (2018-12-05)

- **Breaking Change** replaced `whitespace.openingParenPolicy` and `whitespace.closingParenPolicy` settings with `whitespace.parenConfig` [#282](https://github.com/HaxeCheckstyle/haxe-formatter/issues/282)
- **Breaking Change** replaced `whitespace.openingBracePolicy` `whitespace.closingBracePolicy`, `whitespace.objectOpeningBracePolicy` and `whitespace.objectClosingBracePolicy` settings with `whitespace.bracesConfig` [#282](https://github.com/HaxeCheckstyle/haxe-formatter/issues/282)
- Added check to prevent tokentree parser issues from deleting code [#281](https://github.com/HaxeCheckstyle/haxe-formatter/issues/281)
- Fixed properties in anon types [#276](https://github.com/HaxeCheckstyle/haxe-formatter/issues/276)
- Fixed empty lines between single line types with meta [#277](https://github.com/HaxeCheckstyle/haxe-formatter/issues/277)
- Refactored whitespace handling (space + newline) [#278](https://github.com/HaxeCheckstyle/haxe-formatter/issues/278)
- Refactored coverage report generation [#279](https://github.com/HaxeCheckstyle/haxe-formatter/issues/279)
- Removed superfluous wrapping settings [#280](https://github.com/HaxeCheckstyle/haxe-formatter/issues/280)

## version 1.2.0 (2018-11-12)

- **Breaking Change** replaced `emptyLines.beforeUsing` and `emptyLines.afterImportsUsing` settings with `emptyLines.importAndUsing` [#267](https://github.com/HaxeCheckstyle/haxe-formatter/issues/267)
- Added `wrapping.opBoolChain` for wrapping `||` and `&&` chains [#248](https://github.com/HaxeCheckstyle/haxe-formatter/issues/248)
- Added line count statistic for CLI [#249](https://github.com/HaxeCheckstyle/haxe-formatter/issues/249)
- Added `alignedDecrease` option for `indentation.conditionalPolicy`, fixes [#252](https://github.com/HaxeCheckstyle/haxe-formatter/issues/252) ([#253](https://github.com/HaxeCheckstyle/haxe-formatter/issues/253))
- Added `indentation.indentObjectLiteral` to control indent of object literals, fixes [#258](https://github.com/HaxeCheckstyle/haxe-formatter/issues/258) ([#266](https://github.com/HaxeCheckstyle/haxe-formatter/issues/266))
- Added `sameLine.returnBody` option, fixes [#257](https://github.com/HaxeCheckstyle/haxe-formatter/issues/257) ([#266](https://github.com/HaxeCheckstyle/haxe-formatter/issues/266))
- Added `emptyLines.importAndUsing` for more control over empty lines between imports [#267](https://github.com/HaxeCheckstyle/haxe-formatter/issues/267) +  [#268](https://github.com/HaxeCheckstyle/haxe-formatter/issues/268)
- Added `emptyLines.beforePackage` to add empty lines before package declaration [#267](https://github.com/HaxeCheckstyle/haxe-formatter/issues/267)
- Added `emptyLines.betweenSingleLineTypes` for empty lines between single line types, fixes [#255](https://github.com/HaxeCheckstyle/haxe-formatter/issues/255) ([#269](https://github.com/HaxeCheckstyle/haxe-formatter/issues/269))
- Fixed wrapping of function calls, fixes [#169](https://github.com/HaxeCheckstyle/haxe-formatter/issues/169) ([#248](https://github.com/HaxeCheckstyle/haxe-formatter/issues/248))
- Fixed conditional handling in multi-line object literals, fixes [#182](https://github.com/HaxeCheckstyle/haxe-formatter/issues/182) and [#211](https://github.com/HaxeCheckstyle/haxe-formatter/issues/211) ([#248](https://github.com/HaxeCheckstyle/haxe-formatter/issues/248))
- Fixed indentation and wrapping of OpBool chain, fixes [#187](https://github.com/HaxeCheckstyle/haxe-formatter/issues/187) ([#248](https://github.com/HaxeCheckstyle/haxe-formatter/issues/248))
- Fixed sameline options for `sameLine.caseBody` and `sameLine.expressionCase` [#250](https://github.com/HaxeCheckstyle/haxe-formatter/issues/250)
- Fixed conditional indentation with `indentation.conditionalPolicy` set to `alignedIncrease` [#253](https://github.com/HaxeCheckstyle/haxe-formatter/issues/253)
- Fixed truncation of interface conditionals extends, fixes [#259](https://github.com/HaxeCheckstyle/haxe-formatter/issues/259) ([#260](https://github.com/HaxeCheckstyle/haxe-formatter/issues/260))
- Fixed multiline string interpolation, fixes [#261](https://github.com/HaxeCheckstyle/haxe-formatter/issues/261), fixes [#203](https://github.com/HaxeCheckstyle/haxe-formatter/issues/203) ([#264](https://github.com/HaxeCheckstyle/haxe-formatter/issues/264) + [#265](https://github.com/HaxeCheckstyle/haxe-formatter/issues/265))
- Fixed send error messages to stderr, fixes [#262](https://github.com/HaxeCheckstyle/haxe-formatter/issues/262) ([#265](https://github.com/HaxeCheckstyle/haxe-formatter/issues/265))
- Fixed metadata whitespace, fixes [#263](https://github.com/HaxeCheckstyle/haxe-formatter/issues/263) ([#265](https://github.com/HaxeCheckstyle/haxe-formatter/issues/265))
- Fixed indentation of wrapped anon function, fixes [#256](https://github.com/HaxeCheckstyle/haxe-formatter/issues/256) ([#266](https://github.com/HaxeCheckstyle/haxe-formatter/issues/266))
- Fixed empty lines of doc comments inside conditionals, fixes [#188](https://github.com/HaxeCheckstyle/haxe-formatter/issues/188) ([#270](https://github.com/HaxeCheckstyle/haxe-formatter/issues/270))
- Fixed `sameLine.expression*` handling of Binop and Arrow, fixes [#271](https://github.com/HaxeCheckstyle/haxe-formatter/issues/271) + [#272](https://github.com/HaxeCheckstyle/haxe-formatter/issues/272) ([#274](https://github.com/HaxeCheckstyle/haxe-formatter/issues/274))
- Fixed type hints for arrow function parameters in calls, fixes [#273](https://github.com/HaxeCheckstyle/haxe-formatter/issues/273) ([#274](https://github.com/HaxeCheckstyle/haxe-formatter/issues/274))
- Changed `sameLine.expressionCase` to `keep` [#250](https://github.com/HaxeCheckstyle/haxe-formatter/issues/250)
- Refactored call and parameter wrapping [#247](https://github.com/HaxeCheckstyle/haxe-formatter/issues/247)
- Refactored method chain wrapping [#247](https://github.com/HaxeCheckstyle/haxe-formatter/issues/247)

## version 1.1.2 (2018-10-17)

- Added indentation support for `Binop(OpAssignOp(_))` [#245](https://github.com/HaxeCheckstyle/haxe-formatter/issues/245)
- Fixed endless loop during wrapping of `#if (macro)` [#245](https://github.com/HaxeCheckstyle/haxe-formatter/issues/245)

## version 1.1.1 (2018-10-13)

- Added support for key-value iterators, fixes [#232](https://github.com/HaxeCheckstyle/haxe-formatter/issues/232) ([#233](https://github.com/HaxeCheckstyle/haxe-formatter/issues/233))
- Added `wrapping.anonFunctionSignature` [#239](https://github.com/HaxeCheckstyle/haxe-formatter/issues/239)
- Fixed version number reported on CLI [#233](https://github.com/HaxeCheckstyle/haxe-formatter/issues/233)
- Fixed handling of macro blocks, fixes [#215](https://github.com/HaxeCheckstyle/haxe-formatter/issues/215) [#233](https://github.com/HaxeCheckstyle/haxe-formatter/issues/233)
- Fixed unstable formatting after typedefs without semicolon, fixes [#216](https://github.com/HaxeCheckstyle/haxe-formatter/issues/216) [#233](https://github.com/HaxeCheckstyle/haxe-formatter/issues/233)
- Fixed CLI version checks for existence of `run.js` before invoking node [#234](https://github.com/HaxeCheckstyle/haxe-formatter/issues/234)
- Fixed `keep` option of `sameLine.ifElse`, fixes [#235](https://github.com/HaxeCheckstyle/haxe-formatter/issues/235) ([#236](https://github.com/HaxeCheckstyle/haxe-formatter/issues/236))
- Fixed wrapping in middle of function call, fixes [#180](https://github.com/HaxeCheckstyle/haxe-formatter/issues/180) ([#239](https://github.com/HaxeCheckstyle/haxe-formatter/issues/239))
- Fixed wrapping function signature with long type hint, fixes [#189](https://github.com/HaxeCheckstyle/haxe-formatter/issues/189) ([#239](https://github.com/HaxeCheckstyle/haxe-formatter/issues/239))
- Fixed indentation with comment line, fixes [#237](https://github.com/HaxeCheckstyle/haxe-formatter/issues/237) ([#239](https://github.com/HaxeCheckstyle/haxe-formatter/issues/239))
- Fixed wrapping option keep, fixes [#238](https://github.com/HaxeCheckstyle/haxe-formatter/issues/238) ([#239](https://github.com/HaxeCheckstyle/haxe-formatter/issues/239))
- Fixed handling of metadata parameter, fixes [#241](https://github.com/HaxeCheckstyle/haxe-formatter/issues/241) ([#242](https://github.com/HaxeCheckstyle/haxe-formatter/issues/242))
- Changed default rules for function signature, see [#232](https://github.com/HaxeCheckstyle/haxe-formatter/issues/232) ([#233](https://github.com/HaxeCheckstyle/haxe-formatter/issues/233))
- Refactored marker classes to use a common base class [#239](https://github.com/HaxeCheckstyle/haxe-formatter/issues/239)
- Refactored whitespace policy add/remove handling [#243](https://github.com/HaxeCheckstyle/haxe-formatter/issues/243)
- Refactored indentation logic [#243](https://github.com/HaxeCheckstyle/haxe-formatter/issues/243)
- Removed whitespace change around comments during line end marking [#243](https://github.com/HaxeCheckstyle/haxe-formatter/issues/243)

## version 1.1.0 (2018-09-04)

- **Breaking Change** added a default wrap type for wrapping rules [#230](https://github.com/HaxeCheckstyle/haxe-formatter/issues/230)
- Added `Keep` to SameLinePolicy [#226](https://github.com/HaxeCheckstyle/haxe-formatter/issues/226)
- Added `emptyLines.classEmptyLines.endType` to output empty lines before end of classes [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Added `emptyLines.abstractEmptyLines.endType` to output empty lines before end of abstracts [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Added `emptyLines.enumAbstractEmptyLines.endType` to output empty lines before end of enum abstracts [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Added `emptyLines.externClassEmptyLines.endType` to output empty lines before end of extern classes [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Added `emptyLines.interfaceEmptyLines.endType` to output empty lines before end of interfaces [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Added `emptyLines.enumEmptyLines.endType` to output empty lines before end of enums [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Added `emptyLines.typedefEmptyLines.endType` to output empty lines before end of typedefs [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Added `indentation.trailingWhitespace` to output trailing whitespace in empty lines [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Added `--default-config` CLI option to generate a default "hxformat.json" [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Added `wrapping.methodChaining` to handle wrapping of method chains [#229](https://github.com/HaxeCheckstyle/haxe-formatter/issues/229)
- Changed `whitespace.closingBracePolicy` to `after` [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Changed `whitespace.typeExtensionPolicy` to `after` [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Changed `whitespace.semicolonPolicy` to `onlyAfter` [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)
- Fixed indentation of else with object literal body, fixes [#220](https://github.com/HaxeCheckstyle/haxe-formatter/issues/220) ([#224](https://github.com/HaxeCheckstyle/haxe-formatter/issues/224))
- Fixed indentation of prefix unary, fixes [#221](https://github.com/HaxeCheckstyle/haxe-formatter/issues/221) ([#224](https://github.com/HaxeCheckstyle/haxe-formatter/issues/224))
- Fixed whitespace after macro reification, fixes [#218](https://github.com/HaxeCheckstyle/haxe-formatter/issues/218) + [#219](https://github.com/HaxeCheckstyle/haxe-formatter/issues/219) ([#225](https://github.com/HaxeCheckstyle/haxe-formatter/issues/225))
- Fixed type check on numbers, fixes [#217](https://github.com/HaxeCheckstyle/haxe-formatter/issues/217) ([#225](https://github.com/HaxeCheckstyle/haxe-formatter/issues/225))
- Fixed typedefs without assign, fixes [#228](https://github.com/HaxeCheckstyle/haxe-formatter/issues/228) ([#229](https://github.com/HaxeCheckstyle/haxe-formatter/issues/229))
- Refactored Space and Newline handling of MarkWhitespace [#227](https://github.com/HaxeCheckstyle/haxe-formatter/issues/227)

## version 1.0.0 (2018-08-20)

- initial release
