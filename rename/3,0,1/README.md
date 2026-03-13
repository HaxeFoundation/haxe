# Haxe rename

[![Haxe-Rename](https://github.com/HaxeCheckstyle/haxe-rename/workflows/Haxe-Rename/badge.svg)](https://github.com/HaxeCheckstyle/haxe-rename/actions)
[![codecov](https://codecov.io/gh/HaxeCheckstyle/haxe-rename/branch/master/graph/badge.svg)](https://codecov.io/gh/HaxeCheckstyle/haxe-rename)

a work-in-progress renaming tool for Haxe code.

CAUTION: Make sure you have backups before performing any rename operations!

Note: renaming uses static code analysis to figure out what places to change in a rename operation. if you make heavy use of type inference you might end up with compile errors.

## features

* move a file to a different package
* rename an import alias
* rename a type
* rename a function parameter
* rename a local var
* rename a local function
* remame a case capture
* rename module level statics
* rename interface fields
* rename class fields
* rename enum fields
* rename anon struct fields (limited to object literals)

## usage

```text
Haxe Rename 1.0.0
[-s | --source] <path> : file or directory with .hx files (multiple allowed)
[-l] <location>        : location (path + filename and byte offset from beginning of file) of identifier to rename - <src/pack/Filename.hx@123>
[-n] <newName>         : new name for all occurences of identifier
[-x]                   : perform renaming operations
[--i-have-backups]     : you have a backup and you really, really want to rename
[-h | --help]          : display list of options
```

### dry run

`node bin/rename.js -s src -s test -l src/refactor/Refactor.hx@108 -n Rename`

```haxe
test/refactor/TestBase.hx
* replace text with "refactor.Rename" @48-65
--- import refactor.Refactor;
+++ import refactor.Rename;
* replace text with "Rename" @993-1001
---             var result:RefactorResult = Refactor.refactor({
+++             var result:RefactorResult = Rename.refactor({
src/refactor/Refactor.hx
* rename to "src/refactor/Rename.hx"
* replace text with "Rename" @348-356
--- class Refactor {
+++ class Rename {
src/refactor/Cli.hx
* replace text with "Rename" @2743-2751
---             var result:RefactorResult = Refactor.refactor({
+++             var result:RefactorResult = Rename.refactor({
```

### danger zone

`node bin/rename.js -s src -l path/pack/FileName.hx@600 -n newName -x --i-have-backups`

## compile

```bash
npm install
lix download
haxe build.hxml
```
