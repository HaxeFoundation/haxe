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
import cs.Boot;
import cs.Lib;
import haxe.lang.Exceptions;
 
@:core_api @:nativegen class Std {
	public static function is( v : Dynamic, t : Dynamic ) : Bool 
	{
		var clt:Class<Dynamic> = cast t;
		if (clt == null)
			return false;
		
		var native:cs.native.Type = untyped clt.nativeType();
		
		return native.IsAssignableFrom(Lib.getNativeType(v));
	}

	public static function string( s : Dynamic ) : String {
		return s + "";
	}

	public static inline function int( x : Float ) : Int {
		return cast x;
	}
	
	@:functionBody('
			try 
			{
				return new Haxe.Lang.Null<int>(System.Int32.Parse(x), true);
			} 
			catch (System.FormatException fe)
			{
				return default(Haxe.Lang.Null<int>);
			}
	')
	public static function parseInt( x : String ) : Null<Int> {
		return null;
	}

	@:functionBody('
			try 
			{
				return System.Double.Parse(x);
			} 
			catch (System.FormatException fe)
			{
				return double.NaN;
			}
	')
	public static function parseFloat( x : String ) : Float {
		return null;
	}

	public static function random( x : Int ) : Int {
		return untyped Math.rand.Next(x);
	}

	@:macro public static function format( fmt : haxe.macro.Expr.ExprRequire<String> ) : haxe.macro.Expr.ExprRequire<String> {
		return haxe.macro.Format.format(fmt);
	}

}
	