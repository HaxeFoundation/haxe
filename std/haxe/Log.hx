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
		var str:String = null;
		if (infos != null) {
			str = infos.fileName + ":" + infos.lineNumber + ": " + v;
			if (infos.customParams != null)
			{
				str += "," + infos.customParams.join(",");
			}
		} else {
			str = Std.string(v);
		}
		traceOutput(str);
	}

	/**
		Allows to simply rebind the way all trace calls are output, without changing
		the way the traces are formated.
	**/
	public static dynamic function traceOutput( v : String ) : Void {
		#if sys
		Sys.println(v);
		#elseif js
		if( js.Lib.typeof(untyped console) != "undefined" && (untyped console).log != null )
			(untyped console).log(v);
		#elseif lua
		untyped __define_feature__("use._hx_print",_hx_print(v));
		#else
		throw "Not implemented";
		#end
	}

}
