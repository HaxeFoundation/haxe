/*
 * Copyright (C)2005-2012 Haxe Foundation
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
	The haxe Reflection API allows retrieval of type information at runtime.
	
	This class complements the more lightweight Reflect class, with a focus on
	class and enum instances.
**/
extern class Type {

	/**
		Returns the class of [o], if [o] is a class instance.
		
		If [o] is null or of a different type, null is returned.
		
		In general, type parameter information cannot be obtained at runtime.
	**/
	public static function getClass<T>( o : T ) : Class<T>;

	/**
		Returns the enum of enum instance [o].
		
		An enum instance is the result of using an enum constructor. Given an
		enum Color { Red; }, getEnum(Red) returns Enum<Color>.
		
		If [o] is null, null is returned.
		
		In general, type parameter information cannot be obtained at runtime.
	**/
	public static function getEnum( o : EnumValue ) : Enum<Dynamic>;


	/**
		Returns the super-class of class [c].
		
		If [c] has no super class, null is returned.
		
		If [c] is null, the result is unspecified.
		
		In general, type parameter information cannot be obtained at runtime.
	**/
	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic>;


	/**
		Returns the name of class [c], including its path.
		
		If [c] is inside a package, the package structure is returned dot-
		separated, with another dot separating the class name:
			pack1.pack2.(...).packN.ClassName
		If [c] is a sub-type of a haxe module, that module is not part of the
		package structure.
			
		If [c] has no package, the class name is returned.
		
		If [c] is null, the result is unspecified.
		
		The class name does not include any type parameters.
	**/
	public static function getClassName( c : Class<Dynamic> ) : String;

	/**
		Returns the name of enum [e], including its path.
		
		If [e] is inside a package, the package structure is returned dot-
		separated, with another dot separating the enum name:
			pack1.pack2.(...).packN.EnumName
		If [e] is a sub-type of a haxe module, that module is not part of the
		package structure.
			
		If [e] has no package, the enum name is returned.
		
		If [e] is null, the result is unspecified.
		
		The enum name does not include any type parameters.
	**/
	public static function getEnumName( e : Enum<Dynamic> ) : String;

	/**
		Resolves a class by name.
		
		If [name] is the path of an existing class, that class is returned.
		
		Otherwise null is returned.
		
		If [name] is null or the path to a different type, the result is
		unspecified.
		
		The class name must not include any type parameters.
	**/
	public static function resolveClass( name : String ) : Class<Dynamic>;

	/**
		Resolves an enum by name.
		
		If [name] is the path of an existing enum, that enum is returned.
		
		Otherwise null is returned.
		
		If [name] is null the result is unspecified.
		
		If [name] is the path to a different type, null is returned.
		
		The enum name must not include any type parameters.
	**/
	public static function resolveEnum( name : String ) : Enum<Dynamic>;

	/**
		Creates an instance of class [cl], using [args] as arguments to the
		class constructor.
		
		This function guarantees that the class constructor is called.
		
		Default values of constructors arguments are not guaranteed to be
		taken into account.
		
		If [cl] or [args] are null, or if the number of elements in [args] does
		not match the expected number of constructor arguments, or if [cl] has
		no own constructor, the result is unspecified.
		
		In particular, default values of constructor arguments are not
		guaranteed to be taken into account.
	**/
	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T;
	
	/**
		Creates an instance of class [cl].
		
		This function guarantees that the class constructor is not called.
		
		If [cl] is null, the result is unspecified.
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

