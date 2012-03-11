/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

/**
	The diffent possible runtime types of a value.
	See [Type] for the haXe Reflection API.
**/
enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass( c : Class<Dynamic> );
	TEnum( e : Enum<Dynamic> );
	TUnknown;
}

/**
	The haXe Reflection API enables you to retreive informations about any value,
	Classes and Enums at runtime.
**/
extern class Type {

	/**
		Returns the class of a value or [null] if this value is not a Class instance.
	**/
	public static function getClass<T>( o : T ) : Class<T>;

	/**
		Returns the enum of a value or [null] if this value is not an Enum instance.
	**/
	public static function getEnum( o : EnumValue ) : Enum<Dynamic>;


	/**
		Returns the super-class of a class, or null if no super class.
	**/
	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic>;


	/**
		Returns the complete name of a class.
	**/
	public static function getClassName( c : Class<Dynamic> ) : String;

	/**
		Returns the complete name of an enum.
	**/
	public static function getEnumName( e : Enum<Dynamic> ) : String;

	/**
		Evaluates a class from a name. The class must have been compiled
		to be accessible.
	**/
	public static function resolveClass( name : String ) : Class<Dynamic>;


	/**
		Evaluates an enum from a name. The enum must have been compiled
		to be accessible.
	**/
	public static function resolveEnum( name : String ) : Enum<Dynamic>;

	/**
		Creates an instance of the given class with the list of constructor arguments.
	**/
	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T;
	/**
		Similar to [Reflect.createInstance] excepts that the constructor is not called.
		This enables you to create an instance without any side-effect.
	**/
	public static function createEmptyInstance<T>( cl : Class<T> ) : T;

	/**
		Create an instance of an enum by using a constructor name and parameters.
	**/
	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T;

	/**
		Create an instance of an enum by using a constructor index and parameters.
	**/
	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T;

	/**
		Returns the list of instance fields.
	**/
	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String>;

	/**
		Returns the list of a class static fields.
	**/
	public static function getClassFields( c : Class<Dynamic> ) : Array<String>;

	/**
		Returns all the available constructor names for an enum.
	**/
	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String>;

	/**
		Returns the runtime type of a value.
	**/
	public static function typeof( v : Dynamic ) : ValueType;

	/**
		Recursively compare two enums constructors and parameters.
	**/
	public static function enumEq<T>( a : T, b : T ) : Bool;

	/**
		Returns the constructor of an enum
	**/
	public static function enumConstructor( e : EnumValue ) : String;

	/**
		Returns the parameters of an enum
	**/
	public static function enumParameters( e : EnumValue ) : Array<Dynamic>;

	/**
		Returns the index of the constructor of an enum
	**/
	public static function enumIndex( e : EnumValue ) : Int;

	/**
		Returns the list of all enum values that don't take any parameter.
	**/
	public static function allEnums<T>( e : Enum<T> ) : Array<T>;

}

