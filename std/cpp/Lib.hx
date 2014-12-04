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
package cpp;

#if macro
import haxe.macro.Context;
import haxe.macro.Type;
import haxe.macro.Expr;
#end

using cpp.NativeString;
using cpp.RawConstPointer;
using cpp.Char;

class Lib {

   #if !macro
	/**
		Load and return a Cpp primitive from a DLL library.
	**/
	public static function load( lib : String, prim : String, nargs : Int ) : Dynamic {
		#if (iphone || emscripten)
		return loadLazy(lib,prim,nargs);
		#else
		return untyped __global__.__loadprim(lib,prim,nargs);
		#end
	}

   @:analyzer(no_simplification)
	public static function _loadPrime( lib : String, prim : String, signature : String, quietFail = false ) : Dynamic {
		var factory:Function< RawConstPointer<Char> -> RawPointer<Object> > =
               untyped __global__.__hxcpp_cast_get_proc_address(lib, prim + "__prime", quietFail);
      if (factory!=null)
      {
         var func:Dynamic = factory.call(signature.raw());
         if (func==null && !quietFail)
            throw '$prim does not have signature $signature';
         return func;
      }
      return null;
	}


	/**
		Tries to load, and always returns a valid function, but the function may throw
		if called.
	**/
	public static function loadLazy(lib,prim,nargs) : Dynamic {
		try {
			return untyped __global__.__loadprim(lib,prim,nargs);
		} catch( e : Dynamic ) {
			switch(nargs) {
			case 0 : return function() { throw e; };
			case 2 : return function(_1,_2) { throw e; };
			case 3 : return function(_1,_2,_3) { throw e; };
			case 4 : return function(_1,_2,_3,_4) { throw e; };
			case 5 : return function(_1,_2,_3,_4,_5) { throw e; };
			default : return function(_1) { throw e; };
			}
		}
		return null;
	}

	public static function rethrow(inExp:Dynamic) { throw inExp; }

	public static function stringReference(inExp:Dynamic) { throw inExp; }

	/**
		Print the specified value on the default output.
	**/
	public static function print( v : Dynamic ) : Void {
		untyped __global__.__hxcpp_print(v);
	}

	/**
		This function is used to make porting from neko to cpp easy.
		It does not need to do anything because the c-code can work with any Dynamic
	**/
	public static function haxeToNeko( v : Dynamic ) : Dynamic {
		return v;
	}

	/**
		This function is used to make porting from neko to cpp easy.
		It does not need to do anything because the c-code can work with any Dynamic
	**/
	public static function nekoToHaxe( v : Dynamic ) : Dynamic {
		return v;
	}
	/**
		Print the specified value on the default output followed by a newline character.
	**/
	public static function println( v : Dynamic ) : Void {
		untyped __global__.__hxcpp_println(v);
	}

   #else
   static function codeToType(code:String) : String
   {
      switch(code)
      {
         case "b" : return "Bool";
         case "i" : return "Int";
         case "d" : return "Float";
         case "f" : return "cpp.Float32";
         case "s" : return "String";
         case "o" : return "cpp.Object";
         case "v" : return "cpp.Void";
         case "c" : return "cpp.RawConstPtr<cpp.Char> ";
         default:
            throw "Unknown signature type :" + code;
      }
   }
   #end

   public static macro function loadPrime(inModule:String, inName:String, inSig:String,inAllowFail:Bool = false)
   {
      var parts = inSig.split("");
      if (parts.length<1)
         throw "Invalid function signature " + inSig;
      var typeString = parts.length==1 ? "Void" : codeToType(parts.shift());
      for(p in parts)
         typeString += "->" + codeToType(p);
      typeString = "cpp.Function<" + typeString + ">";
      var expr = 'new $typeString(cpp.Lib._loadPrime("$inModule","$inName","$inSig",$inAllowFail))';
      return Context.parse( expr, Context.currentPos() );
   }

}
