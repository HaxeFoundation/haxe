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

@:coreApi class Log {
	@:native("__trace")
	extern private static function nativeTrace(message:String, posInfo:Dynamic):Void;

	public static dynamic function trace(v:Dynamic, ?infos:PosInfos):Void {
		if (infos != null && infos.customParams != null) {
			var extra:String = "";
			for (v in infos.customParams)
				extra += "," + v;
			nativeTrace(v + extra, infos);
		} else
			nativeTrace(v, infos);
	}

	public static function formatOutput(v:Dynamic, infos:PosInfos):String {
		var str = Std.string(v);
		if (infos == null)
			return str;
		var pstr = infos.fileName + ":" + infos.lineNumber;
		if (infos != null && infos.customParams != null)
			for (v in infos.customParams)
				str += ", " + Std.string(v);
		return pstr + ": " + str;
	}
}
