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
package python.internal;

import python.Syntax;

import python.Syntax.pythonCode in py;

@:noDoc
@:native("HxOverrides")
@:access(python.internal.ArrayImpl)
@:access(python.Boot)
class HxOverrides {

	// this two cases iterator and shift are like all methods in String and Array and are already handled in Reflect
	// we need to modify the transformer to call Reflect directly

	@:ifFeature("dynamic_read.iterator", "anon_optional_read.iterator", "anon_read.iterator")
	static public function iterator(x) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).iterator();
		}
		return Syntax.callField(x, "iterator");
	}
	@:ifFeature("dynamic_binop_==", "dynamic_binop_!=")
	static function eq( a:Dynamic, b:Dynamic ) : Bool {
		if (Boot.isArray(a) || Boot.isArray(b)) {
			return Syntax.pythonCode('a is b');
		}
		return Syntax.binop(a, "==", b);
	}
	@:ifFeature("unsafe_string_concat")
	static function stringOrNull (s:String):String {
		return if (s == null) "null" else s;
	}

	@:ifFeature("dynamic_read.shift", "anon_optional_read.shift", "anon_read.shift")
	static public function shift(x) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).shift();
		}
		return Syntax.callField(x, "shift");
	}

	@:ifFeature("dynamic_read.pop", "anon_optional_read.pop", "anon_read.pop")
	static public function pop(x) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).pop();
		}
		return Syntax.callField(x, "pop");
	}

	@:ifFeature("dynamic_read.push", "anon_optional_read.push", "anon_read.push")
	static public function push(x, e) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).push(e);
		}
		return Syntax.callField(x, "push", e);
	}

	@:ifFeature("dynamic_read.join", "anon_optional_read.join", "anon_read.join")
	static public function join(x, sep) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).join(sep);
		}
		return Syntax.callField(x, "join", sep);
	}

	@:ifFeature("dynamic_read.filter", "anon_optional_read.filter", "anon_read.filter")
	static public function filter(x, f) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).filter(f);
		}
		return Syntax.callField(x, "filter", f);
	}

	@:ifFeature("dynamic_read.map", "anon_optional_read.map", "anon_read.map")
	static public function map(x, f) {
		if (Boot.isArray(x)) {
			return (x:Array<Dynamic>).map(f);
		}
		return Syntax.callField(x, "map", f);
	}

	@:ifFeature("dynamic_read.toUpperCase", "anon_optional_read.toUpperCase", "anon_read.toUpperCase")
	static public function toUpperCase(x) {
		if (Boot.isString(x)) {
			return (x:String).toUpperCase();
		}
		return Syntax.callField(x, "toUpperCase");
	}

	@:ifFeature("dynamic_read.toLowerCase", "anon_optional_read.toLowerCase", "anon_read.toLowerCase")
	static public function toLowerCase(x) {
		if (Boot.isString(x)) {
			return (x:String).toLowerCase();
		}
		return Syntax.callField(x, "toLowerCase");
	}

	@:ifFeature("binop_>>>")
	static public function rshift(val:Int, n:Int) {
		return Syntax.binop(Syntax.binop(val, "%", Syntax.pythonCode("0x100000000")), ">>", n);
	}

	@:ifFeature("binop_%")
	static public function modf(a:Float, b:Float) {
		return Syntax.pythonCode("float('nan') if (b == 0.0) else a % b if a >= 0 else -(-a % b)");
	}
	@:ifFeature("binop_%")
	static public function mod(a:Int, b:Int) {
		return Syntax.pythonCode("a % b if a >= 0 else -(-a % b)");
	}

	@:ifFeature("dynamic_array_read")
	static public function arrayGet<T>(a:Dynamic, i:Int):Dynamic {
		if (Boot.isArray(a)) {
			return ArrayImpl._get(a, i);
		} else {
			return Syntax.arrayAccess(a, i);
		}
	}

	@:ifFeature("dynamic_array_write")
	static public function arraySet(a:Dynamic, i:Int, v:Dynamic) {
		if (Boot.isArray(a)) {
			return ArrayImpl._set(a, i, v);
		} else {
			Syntax.assign(Syntax.arrayAccess(a, i), v);
			return v;
		}
	}

	@:ifFeature("python._KwArgs.KwArgs_Impl_.fromT")
	static public function mapKwArgs(a:{}, v:Dict<String,String>)
	{
		var a = python.Lib.dictAsAnon(python.Lib.anonToDict(a));
		for (k in v.keys()) {
			var val = v.get(k);
			if (UBuiltins.hasattr(a, k)) {
				var x = UBuiltins.getattr(a, k);
				UBuiltins.setattr(a, val, x);
				UBuiltins.delattr(a, k);
			}
		}
		return a;
	}

	@:ifFeature("python._KwArgs.KwArgs_Impl_.toDictHelper")
	static public function reverseMapKwArgs(a:Dict<String,Dynamic>, v:Dict<String,String>)
	{
		var a = a.copy();
		for (k in v.keys()) {

			var val = v.get(k);
			if (a.hasKey(val)) {
				var x = a.get(val, null);
				a.set(k, x);
				a.remove(val);
			}
		}
		return a;
	}

}