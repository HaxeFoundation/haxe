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

import haxe.Constraints.Function;
import js.lib.Object;

/**
	`Reflect` is a built-in object that provides methods for interceptable JavaScript operations.
	The methods are the same as those of [proxy handlers](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy/handler).
	Reflect is not a function object, so it's not constructible.

	Documentation [Reflect](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Reflect) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Reflect$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Reflect")
extern class Reflect {
	/**
		Calls a target function with arguments as specified by the args parameter.
		See also `Function.prototype.apply()`.
	 */
	static function apply<T>(target:Function, thisArgument:{}, argumentsList:Array<Any>):T;

	/**
		The `new` operator as a function. Equivalent to calling `new target(...args)`.
		Provides also the optional possibility to specify a different prototype.
	 */
	static function construct<T, S:T>(target:Class<T>, argumentsList:Array<Any>, ?newTarget:Class<S>):T;

	/**
		Similar to `Object.defineProperty()`. Returns a Bool.
	 */
	@:overload(function(target:{}, propertyKey:Symbol, attributes:ObjectPropertyDescriptor<Any>):Bool {})
	static function defineProperty(target:{}, propertyKey:String, attributes:ObjectPropertyDescriptor<Any>):Bool;

	/**
		The `delete` operator as a function. Equivalent to calling `delete target[name]`.
	 */
	@:overload(function<T>(target:Array<T>, propertyKey:Int):Bool {})
	@:overload(function<T>(target:{}, propertyKey:Symbol):Bool {})
	static function deleteProperty(target:{}, propertyKey:String):Bool;

	/**
		A function that returns the value of properties.
	 */
	@:overload(function<T>(target:Array<T>, propertyKey:Int, ?receiver:{}):Null<T> {})
	@:overload(function<T>(target:{}, propertyKey:Symbol, ?receiver:{}):Null<T> {})
	@:pure static function get<T>(target:{}, propertyKey:String, ?receiver:{}):Null<T>;

	/**
		Similar to `Object.getOwnPropertyDescriptor()`.
		Returns a property descriptor of the given property if it exists on the object,
		`undefined` otherwise.
	 */
	@:overload(function<T>(target:Array<T>, propertyKey:Int):Null<ObjectPropertyDescriptor<Any>> {})
	@:overload(function(target:{}, propertyKey:Symbol):Null<ObjectPropertyDescriptor<Any>> {})
	@:pure static function getOwnPropertyDescriptor(target:{}, propertyKey:String):Null<ObjectPropertyDescriptor<Any>>;

	/**
		Same as `Object.getPrototypeOf()`.
	 */
	@:pure static function getPrototypeOf<TProto:{}>(target:{}):Null<TProto>;

	/**
		The `in` operator as function. Returns a boolean indicating whether an own
		or inherited property exists.
	 */
	@:overload(function<T>(target:Array<T>, propertyKey:Int):Bool {})
	@:overload(function(target:{}, propertyKey:Symbol):Bool {})
	@:pure static function has(target:{}, propertyKey:String):Bool;

	/**
		Same as `Object.isExtensible()`.
	 */
	@:pure static function isExtensible(target:{}):Bool;

	/**
		Returns an array of the target object's own (not inherited) property keys.
	 */
	@:pure static function ownKeys(target:{}):Array<String>;

	/**
		Similar to `Object.preventExtensions()`. Returns a Bool.
	 */
	static function preventExtensions(obj:{}):Bool;

	/**
		A function that assigns values to properties. Returns a Bool that is true
		if the update was successful.
	 */
	@:overload(function<T>(target:Array<T>, propertyKey:Int, value:T, ?receiver:{}):Bool {})
	@:overload(function<T>(target:{}, propertyKey:Symbol, value:T, ?receiver:{}):Bool {})
	static function set<T>(target:{}, propertyKey:String, value:T, ?receiver:{}):Bool;

	/**
		A function that sets the prototype of an object.
	 */
	static function setPrototypeOf<TProto:{}>(target:{}, prototype:TProto):Bool;
}
