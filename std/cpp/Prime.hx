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
package cpp;

#if macro
import haxe.macro.Context;
import haxe.macro.Type;
import haxe.macro.Expr;
#end

@:noPackageRestrict
class Prime {

   #if (!macro && cpp)

	public static function _loadPrime( lib : String, prim : String, signature : String, quietFail = false ) : Dynamic {
		var factory:Callable< ConstCharStar -> Object > =
               untyped __global__.__hxcpp_cast_get_proc_address(lib, prim + "__prime", quietFail);
      if (factory!=null)
      {
         var func:Dynamic = factory.call(signature);
         if (func==null && !quietFail)
            throw '$prim does not have signature $signature';
         return func;
      }
      return null;
	}
   #end

   #if (macro)
   static function codeToType(code:String,forCpp:Bool) : String
   {
      var isCpp = Context.defined("cpp");

      switch(code)
      {
         case "b" : return "Bool";
         case "i" : return "Int";
         case "d" : return "Float";
         case "s" : return "String";
         case "f" : return forCpp ? "cpp.Float32" : "Float";
         case "o" : return forCpp ? "cpp.Object" : "Dynamic";
         case "v" : return forCpp ? "cpp.Void" : "Dynamic";
         case "c" :
             if (forCpp)
                return "cpp.ConstCharStar";
             throw "const char * type only supported in cpp mode";
         default:
            throw "Unknown signature type :" + code;
      }
   }
   #end

   public static function nekoInit(inModuleName:String) : Bool
   {
      #if neko
      var init = neko.Lib.load(inModuleName, "neko_init", 5);

      if (init != null)
      {
         init( function(s) return new String(s),
               function(len:Int) { var r = []; if (len > 0) r[len - 1] = null; return r; },
               null,
               true,
               false);
         return true;

      }
      #end
      return false;
   }


   public static macro function load(inModule:String, inName:String, inSig:String,inAllowFail:Bool = false)
   {
      var parts = inSig.split("");
      if (parts.length<1)
         throw "Invalid function signature " + inSig;
      var argCount = parts.length-1;

      var cppMode = Context.defined("cpp");

      var typeString = parts.length==1 ? "Void" : codeToType(parts.shift(),cppMode);
      for(p in parts)
         typeString += "->" + codeToType(p,cppMode);

      if (cppMode)
      {
         typeString = "cpp.Callable<" + typeString + ">";
         var expr = 'new $typeString(cpp.Prime._loadPrime("$inModule","$inName","$inSig",$inAllowFail))';
         return Context.parse( expr, Context.currentPos() );
      }
      else
      {
         if (argCount>5)
            argCount = -1;
         var lazy = inAllowFail ? "loadLazy" : "load";
         var expr = 'new cpp.Callable<$typeString>(neko.Lib.$lazy("$inModule","$inName",$argCount))';
         return Context.parse( expr, Context.currentPos() );
      }
   }

}
