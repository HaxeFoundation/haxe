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

package js.lib;

import js.lib.Object;
import haxe.extern.EitherType;

/**
	The `Proxy` object is used to define custom behavior for fundamental operations
	(e.g. property lookup, assignment, enumeration, function invocation, etc).

	Documentation [Proxy](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Proxy")
extern class Proxy<T:{}> {
	@:pure function new(target:T, handler:ProxyHandler<T>);

	/**
		Creates a revocable `Proxy` object.
	**/
	@:pure static function revocable<T:{}>(target:T, handler:ProxyHandler<T>):RevocableProxy<T>;
}

typedef ProxyHandler<T:{}> = {
	/**
		A trap for `Object.getPrototypeOf`.
	**/
	var ?getPrototypeOf:(target:T) -> Null<{}>;

	/**
		A trap for `Object.setPrototypeOf`.
	**/
	var ?setPrototypeOf:(target:T, prototype:Null<{}>) -> Bool;

	/**
		A trap for `Object.isExtensible`.
	**/
	var ?isExtensible:(target:T) -> Bool;

	/**
		A trap for `Object.preventExtensions`.
	**/
	var ?preventExtensions:(target:T) -> Bool;

	/**
		A trap for `Object.getOwnPropertyDescriptor`.
	**/
	var ?getOwnPropertyDescriptor:(target:T, prop:EitherType<String, Symbol>) -> Null<ObjectPropertyDescriptor<Any>>;

	/**
		A trap for `Object.defineProperty`.
	**/
	var ?defineProperty:(target:T, property:EitherType<String, Symbol>, descriptor:ObjectPropertyDescriptor<Any>) -> Bool;

	/**
		A trap for the `in` operator.
	**/
	var ?has:(target:T, prop:EitherType<String, EitherType<Int, Symbol>>) -> Bool;

	/**
		A trap for getting property values.
	**/
	var ?get:(target:T, property:EitherType<String, EitherType<Int, Symbol>>, receiver:Null<{}>) -> Any;

	/**
		A trap for setting property values.
	**/
	var ?set:(target:T, property:EitherType<String, EitherType<Int, Symbol>>, value:Any, receiver:Null<{}>) -> Bool;

	/**
		A trap for the `delete` operator.
	**/
	var ?deleteProperty:(target:T, property:EitherType<String, EitherType<Int, Symbol>>) -> Bool;

	/**
		A trap for `Object.getOwnPropertyNames` and `Object.getOwnPropertySymbols`.
	**/
	var ?ownKeys:(target:T) -> Array<String>;

	/**
		A trap a function call.
	**/
	var ?apply:(target:T, thisArg:{}, argumentsList:Array<Any>) -> Any;

	/**
		A trap for the `new` operator.
	**/
	var ?construct:(target:Class<T>, argumentsList:Array<Any>, newTarget:Class<Any>) -> Void;
}

typedef RevocableProxy<T:{}> = {
	/**
		A Proxy object created with `new Proxy(target, handler)` call.
	**/
	final proxy:Proxy<T>;

	/**
		A function with no argument to invalidate (switch off) the `proxy`.
	**/
	function revoke():Void;
}
