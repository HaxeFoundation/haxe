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
package haxe;

class Log {

	public static dynamic function trace( v : Dynamic, ?infos : PosInfos ) : Void {
		#if flash
			#if (fdb || nativeTrace)
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
