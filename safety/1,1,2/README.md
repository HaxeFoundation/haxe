# Safety [![Build Status](https://travis-ci.org/RealyUniqueName/Safety.svg?branch=master)](https://travis-ci.org/RealyUniqueName/Safety)

This library implements a few features for writing Haxe code which generates less null pointer errors. These features are: static extensions for more enjoyable null safety, safe navigation operator, safe arrays, safe api (automatic checks of method arguments for `null`).

## Installation

Haxe 4 is required.

Install Safety from haxelib:
```
haxelib install safety
```
or the latest development version from github:
```
haxelib git safety https://github.com/RealyUniqueName/Safety.git
```

## Usage

Add `-lib safety` to your hxml file.
Use following compiler arguments:

* `--macro Safety.safeNavigation(dotPath, recursive)` - Enables [safe navigation operator](https://en.wikipedia.org/wiki/Safe_navigation_operator) `!.` in the specified path. If `recursive` is `true` (it is by default), then `!.` operator is also enabled for subpackages in `dotPath`.
* `--macro Safety.safeArray(dotPath, recursive)` - Makes all array declarations to be typed as `SafeArray`. See [feature description](#safe-array) for details.
* `--macro Safety.safeApi(dotPath, recursive)` - Adds runtime checking for not-nullable arguments of public methods in the specified path. If `null` is passed to such an argument, then `safety.IllegalArgumentException` is thrown. [Details](#safe-api)

All `--macro Safety.*` arguments can be used multiple times with different `dotPath` values.

You can pass empty string `""` as any `dotPath` to apply a feature to the whole codebase (not recommended, because a lot of compilation errors will come from std lib).

## Features

### Static extensions for null safety

Usage example:
```haxe
using Safety;

var nullable:Null<String> = getSomeStr();
var s:String = nullable.or('hello');
```
Available extensions:
```haxe
/**
*  Returns `value` if it is not `null`. Otherwise returns `defaultValue`.
*/
static public inline function or<T>(value:Null<T>, defaultValue:T):T;
/**
*  Returns `value` if it is not `null`.  calls `getter` and returns the result.
*/
static public inline function orGet<T>(value:Null<T>, getter:Void->T):T;
/**
*  Returns `value` if it is not `null`. Otherwise throws an exception.
*  @throws safety.NullPointerException if `value` is `null`.
*/
static public inline function sure<T>(value:Null<T>):T;
/**
*  Just returns `value` without any checks, but typed as not-nullable. Use at your own risk.
*/
static public inline function unsafe<T>(value:Null<T>):T;
/**
*  Returns `true` if value is not null and `callback(value)` is evaluated to `true`.
*  Returns `false` otherwise.
*/
static public inline function check<T>(value:Null<T>, callback:T->Bool):Bool
/**
*  Applies `callback` to `value` and returns the result if `value` is not `null`.
*  Returns `null` otherwise.
*/
static public inline function let<T,V>(value:Null<T>, callback:T->V):Null<V>;
/**
*  Passes `value` to `callback` if `value` is not null.
*/
static public inline function run<T>(value:Null<T>, callback:T->Void):Void;
/**
*  Applies `callback` to `value` if `value` is not `null`.
*  Returns `value`.
*/
static public inline function apply<T>(value:Null<T>, callback:T->Void):Null<T>;
```

### Safe navigation operator

Adds safe navigation operator `!.` to Haxe syntax. Compiler argument to enable it: `--macro Safety.safeNavigation(my.pack)`

```haxe
var obj:Null<{ field:Null<String> }> = null;
trace(obj!.field!.length); //null
obj = { field:'hello' };
trace(obj!.field!.length); //5
```

### Safe array

`SafeArray<T>` (abstract over `Array<T>`) behaves exactly like `Array<T>` except it prevents out-of-bounds reading/writing (throws `safety.OutOfBoundsException`). Writing at an index of array's length is allowed.

See [Null safety limitations](#null-safety-limitations) to find out why you need it.

If `--macro Safety.safeArray(my.pack)` is in effect, then all array declarations become `SafeArray`:
```haxe
var a = ['hello', 'world'];
$type(a); //SafeArray<String>
```
You can use `stdArray()` method to get a reference to the `Array<T>`:
```haxe
var a = ['hello', 'world'];
$type(a.stdArray()); //Array<String>
```

### Safe api

Compiler argument to enable this feature: `--macro Safety.safeApi(my.pack)`.

If enabled it adds runtime checking against `null` for all not-nullable arguments of public methods:
```haxe
public function method(arg:String) {}
<...>
method(null); //throws safety.IllegalArgumentException
```
It's pretty cheap performance wise, because it just adds a simple line to the body of a method: `if(arg == null) throw new IllegalArgumentException();`

If argument is nullable, no check is generated.

Also safe api does not generate such checks for `Int`, `Float`, `Bool` (and other basic types) on static targets, because it's impossible to assign `null` to such types on static targets.

This feature is especially useful if you are creating a library, and you don't want your users to pass `null`s to your API. You can add `--macro Safety.safeApi('my.lib')` to `extraParams.hxml`.

## Null safety Limitations

* Out-of-bounds array read returns `null`, but Haxe types it without `Null<>`. ([PR to the compiler to fix this issue](https://github.com/HaxeFoundation/haxe/pull/6825))
```haxe
var a:Array<String> = ["hello"];
$type(a[100]); // String
trace(a[100]); // null
var s:String = a[100]; // null-safety does not complain here, because `a[100]` is not `Null<String>`, but just `String`
```
* Out-of-bounds array write fills all positions between the last defined index and the newly written one with `null`. null-safety cannot save you in this case.
```haxe
var a:Array<String> = ["hello"];
a[2] = "world";
trace(a); //["hello", null, "world"]
var s:String = a[1]; //null-safety cannot check this
trace(s); //null
```
* Haxe was not designed with null safety in mind, so it's always possible `null` will come to your code from 3rd-party code or even from std lib.
* Nullable fields and properties are not considered null-safe even after checking against `null`. Use safety extensions instead:
```haxe
using Safety;

class Main {
    var nullable:Null<String>;
    function new() {
        var str:String;
        if(nullable != null) {
            str = nullable; //Compilation error.
        }
        str = nullable.sure();
        str = nullable.or('hello');
    }
}
```