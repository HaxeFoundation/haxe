/*
 * Copyright (C)2005-2019 Haxe Foundation
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
	public static function formatOutput(v:Dynamic, infos:Null<PosInfos>):String {
		var str = Std.string(v);
		if (infos == null)
			return str;
		// PosInfos is a typedef mapping to an anonymous type, which becomes 'object' in C#.
		// Null<object> is stripped to just 'object' since object is inherently nullable.
		// So 'infos' is directly the HaxeDynamicObject, no .value unwrapping needed.
		var dynInfos:Dynamic = infos;
		var fileName:String = cs.Syntax.code("(string)((haxe.lang.HaxeDynamicObject){0})._hx_getField(\"fileName\")", dynInfos);
		var lineNumber:Dynamic = cs.Syntax.code("((haxe.lang.HaxeDynamicObject){0})._hx_getField(\"lineNumber\")", dynInfos);
		var pstr = fileName + ":" + Std.string(lineNumber);
		var customParams:Dynamic = cs.Syntax.code("((haxe.lang.HaxeDynamicObject){0})._hx_getField(\"customParams\")", dynInfos);
		if (customParams != null) {
			var arr:Array<Dynamic> = cs.Syntax.code("(haxe.root.Array){0}", customParams);
			for (item in arr)
				str += ", " + Std.string(item);
		}
		return pstr + ": " + str;
	}

	public static dynamic function trace(v:Dynamic, ?infos:PosInfos):Void {
		var str = formatOutput(v, infos);
		Sys.println(str);
	}
}
