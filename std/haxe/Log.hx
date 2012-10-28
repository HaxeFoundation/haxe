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
package haxe;

class Log {

	public static dynamic function trace( v : Dynamic, ?infos : PosInfos ) : Void {
		#if flash
			#if (fdb || native_trace)
		var pstr = infos == null ? "(null)" : infos.fileName+":"+infos.lineNumber;
		untyped #if flash9 __global__["trace"] #else __trace__ #end(pstr+": "+flash.Boot.__string_rec(v,""));
			#else
		untyped flash.Boot.__trace(v,infos);
			#end
		#elseif neko
		untyped __dollar__print(infos.fileName+":"+infos.lineNumber+": ",v,"\n");
		#elseif js
		untyped js.Boot.__trace(v,infos);
		#elseif php
		untyped __call__('_hx_trace', v,infos);
		#elseif cpp
		untyped __trace(v,infos);
		#elseif cs
		var str = infos.fileName + ":" + infos.lineNumber + ": " + v;
		untyped __cs__("System.Console.WriteLine(str)");
		#elseif java
		var str = infos.fileName + ":" + infos.lineNumber + ": " + v;
		untyped __java__("java.lang.System.out.println(str)");
		#end
	}

	public static dynamic function clear() : Void {
		#if flash
		untyped flash.Boot.__clear_trace();
		#elseif js
		untyped js.Boot.__clear_trace();
		#end
	}

	#if flash
	public static dynamic function setColor( rgb : Int ) {
		untyped flash.Boot.__set_trace_color(rgb);
	}
	#end

}
