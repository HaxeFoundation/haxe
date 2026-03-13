# Haxe Formatter

[![Haxelib Version](https://img.shields.io/github/tag/HaxeCheckstyle/haxe-formatter.svg?label=haxelib)](http://lib.haxe.org/p/formatter)
[![Build Status](https://img.shields.io/github/actions/workflow/status/HaxeCheckstyle/haxe-formatter/formatter.yml)](https://github.com/HaxeCheckstyle/haxe-formatter/actions)
[![Haxe-Formatter](https://github.com/HaxeCheckstyle/haxe-formatter/workflows/Haxe-Formatter/badge.svg)](https://github.com/HaxeCheckstyle/haxe-formatter/actions)
[![codecov](https://codecov.io/gh/HaxeCheckstyle/haxe-formatter/branch/master/graph/badge.svg)](https://codecov.io/gh/HaxeCheckstyle/haxe-formatter)

A Haxe Code Formatter based on the [tokentree](https://github.com/HaxeCheckstyle/tokentree) library.

![VSCode format on save](resources/formatOnSave.gif)

**It's recommended to make a backup of your files and use a diff tool.**

## Features

- Formatting handles:
  - Indentation
  - Whitespace
  - Wrapping
  - Empty Lines
  - Same Line
  - Line Ends
- `hxformat.json` configuration files
- supports latest Haxe 4 syntax
- supports conditional compilation
- [Visual Studio Code Integration](https://github.com/vshaxe/vshaxe/wiki/Formatting)

## Usage

Apart from IDE integration, there's also a command line version of haxe-formatter that can be installed from haxelib:

```bash
haxelib install formatter
```

### Bulk Formatting

- Run `haxelib run formatter -s src` to format folder `src`.
- Run `haxelib run formatter -s src -s test` to format folders `src` and `test`.

Note: haxelib version of formatter comes with both a NodeJS and a Neko version. when running `haxelib run formatter` it always launches Neko version (`run.n`). however Neko version will check for NodeJS (simply by calling `node -v` and checking its exit code). if you have NodeJS installed and working, then it will spawn a new process running `run.js` and passing all of your command line parameters to it. so you might end up running Neko or NodeJS versions of formatter depending on your system setup and installed software.
NodeJS version is noticeably faster than Neko. so it's the preferred version to run when bulk formatting larger code bases.
if you see dramatically slower performance when formatting the same codebase on multiple machines, it might be caused by one of the systems not being able to switch to NodeJS version and doing all the work in Neko instead.

### Single File Formatting

Run `haxelib run formatter -s src/Main.hx` to format file `src/Main.hx`.

### Format check

Use `--check` to run formatter in check mode to see if code is properly formatted without modifying it. This can be especially useful in a CI environment.

### Piped mode

You can run formatter in "piped mode", where it reads code from STDIN and prints formatted results to STDOUT.
You can enable piped mode by giving `--stdin` on command line. In piped mode formatter requires exactly one `--source <path>` to make sure configuration file detection knows where to start.

Formatter does not support a stream mode, where you can provide an endless stream of code to be formatted, your input data needs some sort of end of file.

Formatter will print formatted code to STDOUT.

In case of errors, it will print your input file without modifications (as long as formatter is able to read your input). When an error occurs formatter sets an exit code and prints an error message to STDERR.

| Exit code | Description                                                   | STDOUT                    |
|:---------:| ------------------------------------------------------------- | ------------------------- |
| 0         | Formatting succeeded                                          | Formatted code            |
| -1        | no input data or other unknwon error                          | there might not be output |
| 1         | Formatting is disabled for `--source <path>`                  | unformatted input         |
| 2         | Formatter error                                               | unformatted input         |
| 3         | no reference or invalid path specified via  `--source <path>` | unformatted input         |

Sample call: `haxelib run formatter --stdin -s src/Main.hx < /tmp/code.txt > src/FormattedCode.hx`

## Configuration

Formatter uses `hxformat.json` files for configuration. It searches for a `hxformat.json` file closest to the file being formatted, starting with the file's folder and moving upward all the way to your root folder. A configuration file in a subfolder will always overwrite any settings from a top or higher level folder.

You can test different configuration settings in our [Formatter code samples / playground / documentation](https://haxecheckstyle.github.io/haxe-formatter-docs/#codesamples.CommonSamples.allman_curlies) website.

The VSCode extension comes with a JSON schema providing completion and limited documentation for edition `hxformat.json` files:

![JSON schema for hxformat.json in VSCode](resources/schema.png)

An empty `hxformat.json` (containing only `{}`) or having `hxformat.json` will result in formatting using the built-in default which is the coding style of formatter itself.

When creating your custom `hxformat.json` file, you only need to provide settings that you want to override with respect to the built-in default. So configuration always starts with the default style of formatter. The following example changes curly braces placement and
indentation to 4 space characters:

```json
{
    "lineEnds": {
        "blockCurly": {
            "leftCurly": "both",
            "emptyCurly": "noBreak"
        }
    },
    "indentation": {
        "character": "    "
    }
}
```

Pro tip: Visual Studio Code has `Format on Save` and `Format on Paste` settings that you can enable by adding

```json
"[haxe]": {
  "editor.formatOnSave": true,
  "editor.formatOnPaste": true
}
```

to your configuration (local or global). `formatOnPaste` will also give you `Format Selection` in `.hx` files.

### Ways to opt-out of formatting

- turn off formatter in your IDE / don't run CLI version
  - affects all files
- place a `hxformat.json` file with `{ "disableFormatting": true }` in you workspace
  - affects all files and subfolders from where you placed `hxformat.json`
  - since formatter searches for a `hxformat.json` file closest to the file being formatted, you can `disableFormatting` in a subfolder, while enabling it everywhere else
  - you can reverse a `disableFormatting` by placing a `hxformat.json` with `{}` (for built-in formatter config) in a subfolder, every file in that subfolder and below will get formatted
- use `excludes` inside your `hxformat.json` to specify a number of regexes, that will exclude any filename from formatting matching any one of these regexes
- use `// @formatter:off` and `// @formatter:on` comments inside your code to turn off formatting for parts of your code, it's line based and includes lines containing `// @formatter:off` and `// @formatter:on`.

> NOTE: do not add trailing whitespace to `// @formatter:off` or `// @formatter:on` (they are fine inside a no-format block but not when starting or stopping one)

### How to start using formatter in your projects

Switching from manually formatted source code to an automated formatter workflow can be a lot of work. The following steps should help you get started:

1. make sure you have an unmodified checkout / clone of your project (assuming you are using a VCS)
2. run formatter on your source folder(s) - if you already have a `hxformat.json` formatter will use it, otherwise it will use built-in defaults.
3. use your IDE's diff view, or some other diff viewer to look at the changes formatter made - chances are you don't like what you see, at least for some parts
4. open `hxformat.json` and add / change configuration settings for formatting choices you did not like - we recommend using VSCode, since it has a JSON schema for `hxformat.json` files providing completion support (including (incomplete) documentation for configuration options)
5. goto step 1 and rerun formatter until you are happy with the resulting changelist or run out of options to try - you can rerun formatter on already formatted code, but some options might work better on unmodified sources (e.g. using `keep` in wrapping rules will try to conserve your original line breaks, but it will not recreate them after a previous formatter run removed them)

Depending on the size of your project your initial changelist is going to be large and quite possibly contains every source file.
When you have a `hxformat.json` file that works for you, you can enable formatting in [VSCode](https://github.com/vshaxe/vshaxe/wiki/Formatting) or add an external programm / command that simply calls `haxelib run formatter -s <filename>` (or `node <path_to_formatter>/run.js -s <filename>`) when saving or pressing a hotkey.

## Todo

- improve wrapping

### Compiling formatter

```bash
git clone https://github.com/HaxeCheckstyle/haxe-formatter.git
npm install
lix download
haxe buildAll.hxml # for Neko, NodeJS and Java version + run Unittests and create schema
haxe buildCpp.hxml # for C++ version
```

#### Compiling with Haxe 3

```bash
git clone https://github.com/HaxeCheckstyle/haxe-formatter.git
mv haxe_libraries haxe4_libraries
mv haxe3_libraries haxe_libraries
npm install
lix use haxe 3.4.7
lix download
haxe buildAll.hxml # for Neko, NodeJS and Java version + run Unittests and create schema
haxe buildCpp.hxml # for C++ version
```

### Unittests

To run all unittests simply use `npx haxe test.hxml`

if you want to only run a single testcase you can either:

- use `vshaxe-debug-tools` extension in VSCode which provides a `Run current formatter test` command that runs on any `.hxtest` file
- run it on command line by
  1. place a file called `single-run.txt` in your workspace's `test` folder
  2. make `single-run.txt` contain your testcase's path and name (without `.hxtest` extension) like `test/testcases/sameline/issue_235_keep_if_else`
  3. run `npx haxe test.hxml`
  4. you will get a `test/formatter-result.txt` containing two sections with result and gold (empty sections for green tests)

Removing `test/single-run.txt` makes `haxe test.hxml` do a full run. `vshaxe-debug-tools` is recommended since it performs all manual steps outlined and also opens a diff-view editor so you can easily compare result and gold.
