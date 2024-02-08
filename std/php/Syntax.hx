/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package php;

import haxe.Rest;
import haxe.extern.AsVar;
import haxe.extern.EitherType;

/**
	Special extern class to support PHP language specifics.
	Don't use these functions unless you are really sure what you are doing.
**/
@:noClosure
extern class Syntax {
	/**
		Embeds plain php code.
		`php` should be a string literal with php code.
		It can contain placeholders like `{0}`, `{1}` which will be replaced with corresponding arguments from `args`.
		E.g.:
		```haxe
		Syntax.code("var_dump({0}, {1})", a, b);
		```
		will generate
		```haxe
		var_dump($a, $b);
		```
	**/
	static function code(code:String, args:Rest<Dynamic>):Dynamic;

	/**
		The same as `code()`, but adds dereferencing
		when required to workaround "cannot use temporary expression in write context" php error.
	**/
	static function codeDeref(code:String, args:Rest<Dynamic>):Dynamic;

	/**
		Generates `$left <=> $right`
	**/
	static inline function spaceship(left:Dynamic, right:Dynamic):Int {
		return code('({0} <=> {1})', left, right);
	}

	/**
		Generates `$left ?? $right`
	**/
	static function coalesce<T>(left:T, right:T):T;

	/**
		Generates `$left . $right`
	**/
	static inline function concat(left:String, right:String):String {
		return code('({0} . {1})', left, right);
	}

	/**
		Generates `$left == $right`
	**/
	static inline function equal(left:Dynamic, right:Dynamic):Bool {
		return code('({0} == {1})', left, right);
	}

	/**
		Generates `$left === $right`
	**/
	static inline function strictEqual(left:Dynamic, right:Dynamic):Bool {
		return code('({0} === {1})', left, right);
	}

	/**
		Generates `$left != $right`
	**/
	static inline function notEqual(left:Dynamic, right:Dynamic):Bool {
		return code('({0} != {1})', left, right);
	}

	/**
		Generates `$left !== $right`
	**/
	static inline function strictNotEqual(left:Dynamic, right:Dynamic):Bool {
		return code('({0} !== {1})', left, right);
	}

	/**
		Generates `$left + $right` for numbers.
	**/
	static inline function add<T:Float>(left:T, right:T):T {
		return code('({0} + {1})', left, right);
	}

	/**
		Generates `$left + $right` for php arrays.
	**/
	static inline function union(left:NativeArray, right:NativeArray):NativeArray {
		return codeDeref('({0} + {1})', left, right);
	}

	/**
		Generates `$left ** $right`
	**/
	static inline function exp<T:Float>(left:T, right:T):T {
		return code('({0} ** {1})', left, right);
	}

	/**
		Generates `$left % $right`
	**/
	static inline function mod(left:Float, right:Float):Int {
		return code('({0} % {1})', left, right);
	}

	/**
		Generates `$left ?: $right`
	**/
	static inline function shortTernary<T>(left:T, right:T):T {
		return codeDeref('({0} ?: {1})', left, right);
	}

	/**
		Generates `$left xor $right`
	**/
	static inline function xor(left:Bool, right:Bool):Bool {
		return code('({0} xor {1})', left, right);
	}

	/**
		Generates `(int)$value`
	**/
	static inline function int(value:Dynamic):Int {
		return code('(int)({0})', value);
	}

	/**
		Generates `(float)$value`
	**/
	static inline function float(value:Dynamic):Float {
		return code('(float)({0})', value);
	}

	/**
		Generates `(string)$value`
	**/
	static inline function string(value:Dynamic):String {
		return code('(string)({0})', value);
	}

	/**
		Generates `(bool)$value`
	**/
	static inline function bool(value:Dynamic):Bool {
		return code('(bool)({0})', value);
	}

	/**
		Generates `(object)$value`
	**/
	static inline function object(value:Dynamic):StdClass {
		return codeDeref('((object)({0}))', value);
	}

	/**
		Generates `(array)$value`
	**/
	static inline function array(value:Dynamic):NativeArray {
		return codeDeref('((array)({0}))', value);
	}

	/**
		Generates `$value instanceof $phpClassName`.
		Haxe generates `Std.isOfType(value, Type)` calls as `$value instanceof Type` automatically where possible.
		So you may need this only if you have a `Class` stored in a variable.
	**/
	@:overload(function(value:AsVar<Dynamic>, phpClassName:AsVar<String>):Bool {})
	static function instanceof<V, C>(value:AsVar<V>, type:AsVar<Class<C>>):Bool;

	/**
		Generates PHP class name for a provided Haxe class.
		```haxe
		trace(Syntax.nativeClassName(php.Web)); // outputs: php\Web
		```
	**/
	static function nativeClassName<T>(cls:EitherType<Class<T>, Enum<T>>):String;

	/**
		```haxe
		Syntax.foreach(collection, function(key, value) trace(key, value));
		```
		generates:
		```haxe
		foreach($collection as $key => $value) {
			trace($key, $value);
		}
		```
	**/
	static inline function foreach<TCollection, TKey, TValue>(collection:TCollection, body:TKey->TValue->Void):Void {
		while (Syntax.foreachCondition) {
			Syntax.foreachCollection(collection);
			var key:TKey = Syntax.foreachKey();
			var value:TValue = Syntax.foreachValue();
			Syntax.keepVar(key, value, key, value);
			body(key, value);
		}
	}

	static private var foreachCondition:Bool;
	static private function foreachCollection<T>(collection:T):Void;
	static private function foreachKey<T>():T;
	static private function foreachValue<T>():T;

	/**
		Generates `new $className($arg1, ...$argN)`
	**/
	@:overload(function(className:AsVar<String>, args:Rest<Dynamic>):Dynamic {})
	static function construct<T>(cls:AsVar<Class<T>>, args:Rest<Dynamic>):T;

	/**
		Generates instance field access for reading on `object`
	**/
	static function field<T>(object:AsVar<T>, fieldName:String):Dynamic;

	/**
		Generates instance field access for reading on `object`
	**/
	@:deprecated("php.Syntax.getField() is deprecated. Use php.Syntax.field() instead.")
	static function getField<T>(object:AsVar<T>, fieldName:String):Dynamic;

	/**
		Generates instance field access for writing on `object`
	**/
	static function setField<T>(object:AsVar<T>, fieldName:String, value:Dynamic):Void;

	/**
		Generates static field access for reading on `className`
	**/
	static function getStaticField(className:AsVar<EitherType<Class<Dynamic>, String>>, fieldName:String):Dynamic;

	/**
		Generates static field access for writing on `object`
	**/
	static function setStaticField(object:AsVar<EitherType<Class<Dynamic>, String>>, fieldName:String, value:Dynamic):Void;

	/**
		Generates a call to instance method: `$object->{$methodName}(<args>)`
	**/
	static function call<T>(object:AsVar<T>, methodName:String, args:Rest<Dynamic>):Dynamic;

	/**
		Generates a call to static method: `$className::{$methodName}(<args>)`
	**/
	static function staticCall(className:AsVar<EitherType<Class<Dynamic>, String>>, methodName:String, args:Rest<Dynamic>):Dynamic;

	/**
		```haxe
		Syntax.arrayDecl(arg1, arg2, arg3);
		```
		Generates native array declaration:
		```haxe
		[$arg1, $arg2, $arg3]
		```
	**/
	@:pure
	static function arrayDecl<T>(args:Rest<T>):NativeIndexedArray<T>;

	/**
		```haxe
		Syntax.customArrayDecl([v1 => v2, v3 => v4]);
		```
		Generates native array declaration:
		```haxe
		[$v1 => $v2, $v3 => $v4]
		```
	**/
	macro static function customArrayDecl<T>(decl:haxe.macro.Expr):haxe.macro.Expr.ExprOf<php.NativeArray>;

	/**
		```haxe
		Syntax.assocDecl({field1:'first', field2:2}});
		```
		Generates native associative array declaration:
		```haxe
		["field1" => "first", "field2" => 2];
		```
		This method is not recursive.
		Accepts object declarations only.
		That means you can't pass an object stored in a variable to this method like `Syntax.assocDecl(someVar)`.
		Use `php.Lib.associativeArrayOfObject(someVar)` instead.
	**/
	@:pure
	static function assocDecl<T:{}>(?arg:T):NativeAssocArray<Dynamic>;

	/**
		Don't let compiler to optimize away local var passed to this method.
	**/
	static function keepVar(localVar:Rest<Dynamic>):Void;

	/**
		Adds `...` operator before `args`
	**/
	static inline function splat(args:EitherType<NativeArray, Traversable>):Rest<Dynamic> {
		return code('...{0}', args);
	}

	/**
		Add errors suppression operator `@` before `expression`
	**/
	static function suppress<T>(expression:T):T;

	/**
		Generates `clone $value`.
		@see http://php.net/manual/en/language.oop5.cloning.php
	**/
	static inline function clone<T>(value:T):T {
		return Syntax.code('(clone {0})', value);
	}

	/**
		Generates `yield $value`.
		@see http://php.net/manual/en/language.generators.syntax.php
	**/
	static inline function yield(value:Dynamic):Dynamic {
		return Syntax.code('yield {0}', value);
	}

	/**
		Generates `yield $key => $value`.
		@see http://php.net/manual/en/language.generators.syntax.php
	**/
	static inline function yieldPair(key:Dynamic, value:Dynamic):Dynamic {
		return Syntax.code('yield {0} => {1}', key, value);
	}

	/**
		Generates `yield for $value`.
		@see http://php.net/manual/en/language.generators.syntax.php
	**/
	static inline function yieldFrom(value:Dynamic):Dynamic {
		return Syntax.code('yield from {0}', value);
	}
}
