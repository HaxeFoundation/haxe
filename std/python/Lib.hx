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

package python;

import python.internal.AnonObject;
import python.Dict;
import python.NativeStringTools;
import python.internal.UBuiltins;

typedef PySys = python.lib.Sys;

/**
	Platform-specific Python Library. Provides some platform-specific functions
	for the Python target, such as conversion from Haxe types to native types
	and vice-versa.
**/
class Lib {
	static var lineEnd:String = Sys.systemName() == "Windows" ? "\r\n" : "\n";
	static public var __name__(get, never):String;

	static inline function get___name__():String
		return python.Syntax.code('__name__');

	/**
		Print the specified value on the default output.
	**/
	public static inline function print(v:Dynamic):Void {
		printString(Std.string(v));
	}

	private static function printString(str:String):Void {
		PySys.stdout.buffer.write(NativeStringTools.encode(str, "utf-8"));
		PySys.stdout.flush();
	}

	/**
		Print the specified value on the default output followed by a newline character.
	**/
	public static inline function println(v:Dynamic):Void {
		var str = Std.string(v);
		printString('$str$lineEnd');
	}

	/**
		Returns an anonymous Object which holds the same data as the Dictionary `v`.

		Every key becomes a field of that name: a key that is a Python keyword or starts with `__`,
		like `"from"`, is stored the way Haxe code accesses such a field, so `o.from` reads it.

		If `deep` is true, dictionaries in the values become anonymous objects too, also within
		lists (which are copied).
	**/
	public static function dictToAnon(v:Dict<String, Dynamic>, deep:Bool = false):Dynamic {
		return new AnonObject(fieldsFromDict(v, deep));
	}

	/**
		Returns a copy of the fields of `o` as a Dictionary, with the field names as keys
		(`"from"` for the field `from`, which Python stores under another name). Returns null if `o`
		isn't an anonymous object.

		If `deep` is true, anonymous objects in the values become dictionaries too, also within
		lists and dictionaries (which are copied).
	**/
	@:access(python.Boot.isAnonObject)
	public static function anonToDict(o:{}, deep:Bool = false):Dict<String, Dynamic> {
		return if (Boot.isAnonObject(o)) {
			dictFromFields(Syntax.field(o, "__dict__"), deep);
		} else null;
	}

	/**
		`d` as an anonymous object like `dictToAnon`, but reusing `d` itself unless a key has to be
		renamed (the object hook of `haxe.Json.parse`, where every dictionary is new).
	**/
	@:noCompletion
	@:access(python.Boot.keywords)
	public static function dictToAnonReusing(d:Dict<String, Dynamic>):Dynamic {
		// Checked in Python, not key by key in Haxe code: this runs for every object of a parsed
		// JSON text, and a Haxe loop made parsing over 30 times slower; this is about 20 % slower
		// than the plain copy it replaces. The "__" test also takes keys that don't need renaming
		// ("__x__", or a key containing "\0__"); fieldsFromDict handles those right.
		if (Syntax.code("not {0}.isdisjoint({1}) or '\\0__' in '\\0' + '\\0'.join({1})", Boot.keywords, d))
			return new AnonObject(fieldsFromDict(d, false));
		return new AnonObject(d);
	}

	@:access(python.Boot.handleKeywords)
	static function fieldsFromDict(v:Dict<String, Dynamic>, deep:Bool):Dict<String, Dynamic> {
		var d = new Dict<String, Dynamic>();
		for (k in v.keys()) {
			var x = v.get(k, null);
			d.set(Boot.handleKeywords(k), deep ? toAnonDeep(x) : x);
		}
		return d;
	}

	static function toAnonDeep(v:Dynamic):Dynamic {
		// Only dictionaries with string keys can be objects; others stay dictionaries.
		if (UBuiltins.isinstance(v, Dict) && Syntax.code("all(isinstance(k, str) for k in {0})", v))
			return new AnonObject(fieldsFromDict(v, true));
		if (UBuiltins.isinstance(v, Array))
			return [for (x in (v : Array<Dynamic>)) toAnonDeep(x)];
		return v;
	}

	@:access(python.Boot.unhandleKeywords)
	static function dictFromFields(fields:Dict<String, Dynamic>, deep:Bool):Dict<String, Dynamic> {
		var d = new Dict<String, Dynamic>();
		for (k in fields.keys()) {
			// set on the object by _hx_hasattr, not a field
			if (k == "_hx_disable_getattr")
				continue;
			var x = fields.get(k, null);
			d.set(Boot.unhandleKeywords(k), deep ? toDictDeep(x) : x);
		}
		return d;
	}

	@:access(python.Boot.isAnonObject)
	static function toDictDeep(v:Dynamic):Dynamic {
		if (Boot.isAnonObject(v))
			return dictFromFields(Syntax.field(v, "__dict__"), true);
		if (UBuiltins.isinstance(v, Array))
			return [for (x in (v : Array<Dynamic>)) toDictDeep(x)];
		if (UBuiltins.isinstance(v, Dict)) {
			var src:Dict<String, Dynamic> = v;
			var d = new Dict<String, Dynamic>();
			for (k in src.keys())
				d.set(k, toDictDeep(src.get(k, null)));
			return d;
		}
		return v;
	}

	/**
		Returns the underlying Dictionary of the anonymous object `o`.
		Modifications to this dictionary are reflected in the anonymous Object too.

		The keys are the names Python stores the fields under: a field that is a Python keyword or
		starts with `__` has a prefixed key (see `anonToDict` for a copy with the field names).
	**/
	@:access(python.Boot.isAnonObject)
	public static function anonAsDict(o:{}):Dict<String, Dynamic> {
		return if (Boot.isAnonObject(o)) {
			(Syntax.field(o, "__dict__") : Dict<String, Dynamic>);
		} else null;
	}

	/**
		Returns the Dictionary `d` as an anonymous Object.
		Modifications to the object are reflected in the Dictionary too.

		The keys are taken as the names Python stores the fields under: a key like `"from"` (a
		Python keyword) isn't readable as the field `from` (see `dictToAnon`, which renames).
	**/
	public static inline function dictAsAnon(d:Dict<String, Dynamic>):Dynamic {
		return new AnonObject(d);
	}

	/**
		Return Python native iterable from Haxe iterable.
	**/
	public static function toPythonIterable<T>(it:Iterable<T>):python.NativeIterable<T> {
		return {
			__iter__: function() {
				var it1 = it.iterator();
				var self:NativeIterator<T> = null;
				self = new NativeIterator({
					__next__: function():T {
						if (it1.hasNext()) {
							return it1.next();
						} else {
							throw new python.Exceptions.StopIteration();
						}
					},
					__iter__: function() return self
				});
				return self;
			}
		}
	}

	/**
		Return Haxe iterable from Python native iterable.
	**/
	public static inline function toHaxeIterable<T>(it:NativeIterable<T>):HaxeIterable<T> {
		return new HaxeIterable(it);
	}

	/**
		Return Haxe iterator instance from Python native iterable.
	**/
	public static inline function toHaxeIterator<T>(it:NativeIterator<T>):HaxeIterator<T> {
		return new HaxeIterator(it);
	}
}
