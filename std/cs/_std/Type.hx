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

@:core_api class Type {

	public static function getClass<T>( o : T ) : Class<T> untyped 
	{
		return null;
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped 
	{
		return null;
	}


	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> untyped 
	{
		return null;
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		return null;
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String 
	{
		return null;
	}

	public static function resolveClass( name : String ) : Class<Dynamic> untyped 
	{
		return null;
	}


	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped 
	{
		return null;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped 
	{
		return null;
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped 
	{
		return null;
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T 
	{
		return null;
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		return null;
	}

	static function describe( t : Dynamic, fact : Bool ) : Array<String> untyped {
		return null;
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		return null;
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> {
		return null;
	}

	public static function typeof( v : Dynamic ) : ValueType untyped 
	{
		return null;
	}

	public static function enumEq<T>( a : T, b : T ) : Bool untyped 
	{
		return true;
	}

	public static function enumConstructor( e : EnumValue ) : String untyped
	{
		return e.tag;
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> untyped
	{
		return if( e.params == null ) [] else e.params;
	}

	public inline static function enumIndex( e : EnumValue ) : Int  untyped
	{
		return e.index;
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> 
	{
		return null;
	}

}

