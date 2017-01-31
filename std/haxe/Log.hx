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
package haxe;

/**
	Log primarily provides the `trace()` method, which is invoked upon a call to
	`trace()` in Haxe code.
**/
class Log {

	/**
		Outputs `v` in a platform-dependent way.

		The second parameter `infos` is injected by the compiler and contains
		information about the position where the `trace()` call was made.

		This method can be rebound to a custom function:
			var oldTrace = haxe.Log.trace; // store old function
			haxe.Log.trace = function(v, ?infos) {
			  // handle trace
			}
			...
			haxe.Log.trace = oldTrace;

		If it is bound to null, subsequent calls to `trace()` will cause an
		exception.
	**/
	public static dynamic function trace( v : Dynamic, ?infos : PosInfos ) : Void {
		#if flash
			#if (fdb || native_trace)
				var pstr = infos == null ? "(null)" : infos.fileName + ":" + infos.lineNumber;
				var str = flash.Boot.__string_rec(v, "");
				if( infos != null && infos.customParams != null ) for( v in infos.customParams ) str += "," + flash.Boot.__string_rec(v, "");
				untyped __global__["trace"](pstr+": "+str);
			#else
				untyped flash.Boot.__trace(v,infos);
			#end
		#elseif neko
			untyped {
				$print(infos.fileName + ":" + infos.lineNumber + ": ", v);
				if( infos.customParams != null ) for( v in infos.customParams ) $print(",", v);
				$print("\n");
			}
		#elseif js
			untyped js.Boot.__trace(v,infos);
		#elseif (php && php7)
			php.Boot.trace(v, infos);
		#elseif php
			if (infos!=null && infos.customParams!=null) {
				var extra:String = "";
				for( v in infos.customParams )
					extra += "," + v;
				untyped __call__('_hx_trace', v + extra, infos);
			}
			else
				untyped __call__('_hx_trace', v, infos);
		#elseif cpp
			if (infos!=null && infos.customParams!=null) {
				var extra:String = "";
				for( v in infos.customParams )
					extra += "," + v;
				untyped __trace(v + extra,infos);
			}
			else
				untyped __trace(v,infos);
		#elseif (cs || java || lua)
			var str:String = null;
			if (infos != null) {
				str = infos.fileName + ":" + infos.lineNumber + ": " + v;
				if (infos.customParams != null)
				{
					str += "," + infos.customParams.join(",");
				}
			} else {
				str = v;
			}
			#if cs
			cs.system.Console.WriteLine(str);
			#elseif java
			untyped __java__("java.lang.System.out.println(str)");
			#elseif lua
			if (str == null) str = "null";
			untyped __define_feature__("use._hx_print",_hx_print(str));
			#end
		#elseif (python)
			var str:String = null;
			if (infos != null) {
				str = infos.fileName + ":" + Std.string(infos.lineNumber) + ": " + v;
				if (infos.customParams != null) {
					str += "," + infos.customParams.join(",");
				}
			} else {
				str = v;
			}
			python.Lib.println(str);
		#elseif hl
			var pstr = infos == null ? "(null)" : infos.fileName + ":" + infos.lineNumber;
			var str = Std.string(v);
			if( infos != null && infos.customParams != null ) for( v in infos.customParams ) str += "," + Std.string(v);
			Sys.println(pstr+": "+str);
		#end
	}

	#if (flash || js)
	/**
		Clears the trace output.
	**/
	public static dynamic function clear() : Void {
		#if flash
		untyped flash.Boot.__clear_trace();
		#elseif js
		untyped js.Boot.__clear_trace();
		#end
	}
	#end

	#if flash
	/**
		Sets the color of the trace output to `rgb`.
	**/
	public static dynamic function setColor( rgb : Int ) {
		untyped flash.Boot.__set_trace_color(rgb);
	}
	#end

}
