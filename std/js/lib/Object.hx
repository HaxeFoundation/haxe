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

import haxe.extern.Rest;
import haxe.DynamicAccess;

/**
	The `js.lib.Object` constructor creates an object wrapper.

	Documentation [Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).
**/
@:native("Object")
extern class Object {
	/**
		The Object.assign() method is used to copy the values of all enumerable
		own properties from one or more source objects to a target object. It
		will return the target object.

		Note: this is an ES2015 feature

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign
	**/
	static function assign<TSource:{}, TDest:{}>(target:TSource, sources:Rest<{}>):TDest;

	/**
		The Object.create() method create a new object, using an existing object
		to provide the newly created object's __proto__ . (see browser console
		for visual evidence.)

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/create
	**/
	@:pure static function create<T>(proto:Null<{}>, ?propertiesObject:DynamicAccess<ObjectPropertyDescriptor<Any>>):T;

	/**
		The Object.defineProperties() method defines new or modifies existing
		properties directly on an object, returning the object.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperties
	**/
	static function defineProperties<T:{}>(obj:T, props:DynamicAccess<ObjectPropertyDescriptor<Any>>):T;

	/**
		The static method Object.defineProperty() defines a new property directly
		on an object, or modifies an existing property on an object, and returns
		the object.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty
	**/
	@:overload(function<T:{}, TProp>(obj:T, prop:Symbol, descriptor:ObjectPropertyDescriptor<TProp>):T {})
	static function defineProperty<T:{}, TProp>(obj:T, prop:String, descriptor:ObjectPropertyDescriptor<TProp>):T;

	/**
		The Object.entries() method returns an array of a given object's own
		enumerable property [key, value] pairs, in the same order as that
		provided by a for...in loop (the difference being that a for-in loop
		enumerates properties in the prototype chain as well).

		Note: this is an ES2017 feature

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/entries
	**/
	@:pure static function entries<T:{}>(obj:T):Array<ObjectEntry>;

	/**
		The Object.freeze() method freezes an object: that is, prevents new
		properties from being added to it; prevents existing properties from
		being removed; and prevents existing properties, or their enumerability,
		configurability, or writability, from being changed, it also prevents the
		prototype from being changed.
		The method returns the passed object.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/freeze
	**/
	static function freeze<T:{}>(obj:T):T;

	/**
		Returns a new object from an iterable of key-value pairs
		(reverses Object.entries).
	**/
	@:pure static function fromEntries<T:{}>(iterable:Any):T;

	/**
		The Object.getOwnPropertyDescriptor() method returns a property
		descriptor for an own property (that is, one directly present on an
		object and not in the object's prototype chain) of a given object.

		In ES5, if the first argument to this method is not an object (a
		primitive), then it will cause a TypeError. In ES2015, a non-object
		first argument will be coerced to an object at first.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/getOwnPropertyDescriptor
	**/
	@:overload(function(obj:String, prop:Symbol):Null<ObjectPropertyDescriptor<String>> {})
	@:overload(function(obj:String, prop:String):Null<ObjectPropertyDescriptor<String>> {})
	@:overload(function<T>(target:Array<T>, propertyKey:Int):Null<ObjectPropertyDescriptor<T>> {})
	@:overload(function<T, TProp>(obj:T, prop:Symbol):Null<ObjectPropertyDescriptor<TProp>> {})
	@:pure static function getOwnPropertyDescriptor<T, TProp>(obj:T, prop:String):Null<ObjectPropertyDescriptor<TProp>>;

	/**
		The Object.getOwnPropertyDescriptors() method returns all own property
		descriptors of a given object.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/getOwnPropertyDescriptors
	**/
	@:overload(function(target:String):DynamicAccess<ObjectPropertyDescriptor<String>> {})
	@:overload(function<T>(target:Array<T>):DynamicAccess<ObjectPropertyDescriptor<T>> {})
	@:pure static function getOwnPropertyDescriptors<T>(obj:T):DynamicAccess<ObjectPropertyDescriptor<Any>>;

	/**
		The Object.getOwnPropertyNames() method returns an array of all
		properties (including non-enumerable properties except for those which
		use Symbol) found directly upon a given object.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/getOwnPropertyNames
	**/
	@:pure static function getOwnPropertyNames<T:{}>(obj:T):Array<String>;

	/**
		The Object.getOwnPropertySymbols() method returns an array of all symbol
		properties found directly upon a given object.

		Note: this is an ES2015 feature

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/getOwnPropertySymbols
	**/
	@:pure static function getOwnPropertySymbols<T:{}>(obj:T):Array<Symbol>;

	/**
		The Object.getPrototypeOf() method returns the prototype (i.e. the value
		of the internal [[Prototype]] property) of the specified object.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/getPrototypeOf
	**/
	@:pure static function getPrototypeOf<T:{}, TProto>(obj:T):TProto;

	/**
		The Object.is() method determines whether two values are the same value.

		Note: this is an ES2015 feature

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/is
	**/
	@:native("is") @:pure static function isSame<T:{}>(obj1:T, obj2:T):Bool;

	/**
		The Object.is() method determines whether two values are the same value.

		Note: this is an ES2015 feature

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/is
	**/
	@:deprecated("Use Object.isSame()")
	@:pure static function is<T:{}>(obj1:T, obj2:T):Bool;

	/**
		The Object.isExtensible() method determines if an object is extensible
		(whether it can have new properties added to it).

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/isExtensible
	**/
	@:pure static function isExtensible<T:{}>(obj:T):Bool;

	/**
		The Object.isFrozen() determines if an object is frozen.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/isFrozen
	**/
	@:pure static function isFrozen<T:{}>(obj:T):Bool;

	/**
		The Object.isSealed() method determines if an object is sealed.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/isSealed
	**/
	@:pure static function isSealed<T:{}>(obj:T):Bool;

	/**
		The Object.keys() method returns an array of a given object's own
		enumerable properties, in the same order as that provided by a for...in
		loop (the difference being that a for-in loop enumerates properties in
		the prototype chain as well).

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/keys
	**/
	@:pure static function keys<T:{}>(obj:T):Array<String>;

	/**
		The Object.preventExtensions() method prevents new properties from ever
		being added to an object (i.e. prevents future extensions to the object).

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/preventExtensions
	**/
	static function preventExtensions<T:{}>(obj:T):T;

	/**
		The Object.seal() method seals an object, preventing new properties from
		being added to it and marking all existing properties as
		non-configurable. Values of present properties can still be changed as
		long as they are writable.

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/seal
	**/
	static function seal<T:{}>(obj:T):T;

	/**
		The Object.setPrototypeOf() method sets the prototype (i.e., the internal
		[[Prototype]] property) of a specified object to another object or null.

		Note: this is an ES2015 feature

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/setPrototypeOf
	**/
	static function setPrototypeOf<T:{}, TProto:{}>(obj:T, proto:Null<TProto>):T;

	/**
		The Object.values() method returns an array of a given object's own
		enumerable property values, in the same order as that provided by a
		for...in loop (the difference being that a for-in loop enumerates
		properties in the prototype chain as well).

		Note: this is an ES2017 feature

		See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/values
	**/
	@:pure static function values<T:{}>(obj:T):Array<Any>;

	/**
		Allows the addition of properties to all objects of type Object.
	**/
	static var prototype(default, never):ObjectPrototype;

	/**
		The Object constructor creates an object wrapper.
	**/
	@:pure function new(?value:Any);
}

/**
	Type for
	@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object>
**/
typedef ObjectPrototype = {
	/**
		Returns a boolean indicating whether an object contains the specified
		property as a direct property of that object and not inherited through
		the prototype chain.
	**/
	var hasOwnProperty(default, never):Function;

	/**
		Returns a boolean indicating whether the object this method is called
		upon is in the prototype chain of the specified object.
	**/
	var isPrototypeOf(default, never):Function;

	/**
		Returns a boolean indicating if the internal enumerable attribute is set.
	**/
	var propertyIsEnumerable(default, never):Function;

	/**
		Calls `toString()`.
	**/
	var toLocaleString(default, never):Function;

	/**
		Returns a string representation of the object.
	**/
	var toString(default, never):Function;

	/**
		Returns the primitive value of the specified object.
	**/
	var valueOf(default, never):Function;
}

/**
	@see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/defineProperty>
**/
typedef ObjectPropertyDescriptor<TProp> = {
	/**
		`true` if and only if the type of this property descriptor may be
		changed and if the property may be deleted from the corresponding object.
		Defaults to `false`.
	**/
	var ?configurable:Bool;

	/**
		`true` if and only if this property shows up during enumeration of the
		properties on the corresponding object.
		Defaults to `false`.
	**/
	var ?enumerable:Bool;

	/**
		The value associated with the property.
		Can be any valid JavaScript value (number, object, function, etc).
	**/
	var ?value:TProp;

	/**
		`true` if and only if the value associated with the property may be
		changed with an assignment operator.
		Defaults to `false`.
	**/
	var ?writable:Bool;

	/**
		A function which serves as a getter for the property, or `undefined` if
		there is no getter. When the property is accessed, this function is
		called without arguments and with `this` set to the object through which
		the property is accessed (this may not be the object on which the
		property is defined due to inheritance).
		The return value will be used as the value of the property.
	**/
	var ?get:Void->TProp;

	/**
		A function which serves as a setter for the property, or undefined if
		there is no setter. When the property is assigned to, this function
		is called with one argument (the value being assigned to the property)
		and with `this` set to the object through which the property is assigned.
	**/
	var ?set:TProp->Void;
}

/**
	Key/value access helper for `js.lib.Object.entries()`.
**/
abstract ObjectEntry(Array<Any>) {
	public var key(get, never):String;
	public var value(get, never):Any;
	inline function get_key():String
		return this[0];

	inline function get_value():Any
		return this[1];
}
