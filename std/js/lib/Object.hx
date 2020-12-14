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
		Copies the values of all enumerable own properties from one or more
		source objects to a target object.
	**/
	static function assign<T:{}>(target:T, sources:Rest<{}>):T;

	/**
		Creates a new object with the specified prototype object and properties.
	**/
	@:pure static function create<T>(proto:{}, ?propertiesObject:DynamicAccess<ObjectPropertyDescriptor>):T;

	/**
		Adds the named properties described by the given descriptors to an object.
	**/
	static function defineProperties<T:{}>(obj:T, props:DynamicAccess<ObjectPropertyDescriptor>):T;

	/**
		Adds the named property described by a given descriptor to an object.
	**/
	@:overload(function<T:{}>(obj:T, prop:Symbol, descriptor:ObjectPropertyDescriptor):T {})
	static function defineProperty<T:{}>(obj:T, prop:String, descriptor:ObjectPropertyDescriptor):T;

	/**
		Returns an array containing all of the [key, value] pairs of a given
		object's own enumerable string properties.
	**/
	@:pure static function entries(obj:{}):Array<ObjectEntry>;

	/**
		Freezes an object: other code can't delete or change any properties.
	**/
	static function freeze<T:{}>(obj:T):T;

	/**
		Returns a new object from an iterable of key-value pairs
		(reverses Object.entries).
	**/
	@:pure static function fromEntries<T:{}>(iterable:Any):T;

	/**
		Returns a property descriptor for a named property on an object.
	**/
	@:overload(function<T>(target:Array<T>, propertyKey:Int):Null<ObjectPropertyDescriptor> {})
	@:overload(function(obj:{}, prop:Symbol):Null<ObjectPropertyDescriptor> {})
	@:pure static function getOwnPropertyDescriptor(obj:{}, prop:String):Null<ObjectPropertyDescriptor>;

	/**
		Returns an array containing the names of all of the given object's own
		enumerable and non-enumerable properties.
	**/
	@:pure static function getOwnPropertyNames(obj:{}):Array<String>;

	/**
		Returns an array of all symbol properties found directly upon a given object.
	**/
	@:pure static function getOwnPropertySymbols(obj:{}):Array<Symbol>;

	/**
		Returns the prototype of the specified object.
	**/
	@:pure static function getPrototypeOf<TProto:{}>(obj:{}):Null<TProto>;

	/**
		Compares if two values are the same value. Equates all NaN values
		(which differs from both Abstract Equality Comparison and
		Strict Equality Comparison).
	**/
	@:deprecated('js.lib.Object.is is deprecated. Use js.lib.Object.isSame instead.')
	@:pure static function is<T>(value1:T, value2:T):Bool;

	/**
		Compares if two values are the same value. Equates all NaN values
		(which differs from both Abstract Equality Comparison and
		Strict Equality Comparison).
	**/
	@:native('is')
	@:pure static function isSame<T>(value1:T, value2:T):Bool;

	/**
		Determines if extending of an object is allowed.
	**/
	@:pure static function isExtensible(obj:{}):Bool;

	/**
		Determines if an object was frozen.
	**/
	@:pure static function isFrozen(obj:{}):Bool;

	/**
		Determines if an object is sealed.
	**/
	@:pure static function isSealed(obj:{}):Bool;

	/**
		Returns an array containing the names of all of the given object's own
		enumerable string properties.
	**/
	@:pure static function keys(obj:{}):Array<String>;

	/**
		Prevents any extensions of an object.
	**/
	static function preventExtensions<T:{}>(obj:T):T;

	/**
		Prevents other code from deleting properties of an object.
	**/
	static function seal<T:{}>(obj:T):T;

	/**
		Sets the prototype (i.e., the internal Prototype property).
	**/
	static function setPrototypeOf<T:{}>(obj:T, prototype:Null<{}>):T;

	/**
		Returns an array containing the values that correspond to all of
		a given object's own enumerable string properties.
	**/
	@:pure static function values(obj:{}):Array<Any>;

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
typedef ObjectPropertyDescriptor = {
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
	var ?value:Any;

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
	var ?get:Void->Any;

	/**
		A function which serves as a setter for the property, or undefined if
		there is no setter. When the property is assigned to, this function
		is called with one argument (the value being assigned to the property)
		and with `this` set to the object through which the property is assigned.
	**/
	var ?set:Any->Void;
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
