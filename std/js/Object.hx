/*
 * Copyright (C)2005-2017 Haxe Foundation
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
package js;

import haxe.extern.Rest;
import haxe.DynamicAccess;

@:native("Object")
extern class Object {
	static function assign<T:{}>(target:T, sources:Rest<{}>):T;
	@:pure static function create<T>(proto:{}, ?propertiesObject:DynamicAccess<ObjectPropertyDescriptor>):T;
	static function defineProperties<T:{}>(obj:T, props:DynamicAccess<ObjectPropertyDescriptor>):T;
	static function defineProperty<T:{}>(obj:T, prop:String, descriptor:ObjectPropertyDescriptor):T;
	static function freeze<T:{}>(obj:T):T;
	@:pure static function getOwnPropertyDescriptor(obj:{}, prop:String):Null<ObjectPropertyDescriptor>;
	@:pure static function getOwnPropertyNames(obj:{}):Array<String>;
	@:pure static function getOwnPropertySymbols(obj:{}):Array<Symbol>;
	@:pure static function getPrototypeOf<TProto:{}>(obj:{}):Null<TProto>;
	@:pure static function is<T>(value1:T, value2:T):Bool;
	@:pure static function isExtensible(obj:{}):Bool;
	@:pure static function isFrozen(obj:{}):Bool;
	@:pure static function isSealed(obj:{}):Bool;
	@:pure static function keys(obj:{}):Array<String>;
	static function preventExtensions<T:{}>(obj:T):T;
	static function seal<T:{}>(obj:T):T;
	static function setPrototypeOf<T:{}>(obj:T, prototype:Null<{}>):T;
	static var prototype(default,never):ObjectPrototype;
	@:pure function new(?value:Any);
}

typedef ObjectPrototype = {
	var hasOwnProperty(default,never):Function;
	var isPrototypeOf(default,never):Function;
	var propertyIsEnumerable(default,never):Function;
	var toLocaleString(default,never):Function;
	var toString(default,never):Function;
	var valueOf(default,never):Function;
}

typedef ObjectPropertyDescriptor = {
	@:optional var configurable:Bool;
	@:optional var enumerable:Bool;
	@:optional var value:Any;
	@:optional var writable:Bool;
	@:optional var get:Void->Any;
	@:optional var set:Any->Void;
}
