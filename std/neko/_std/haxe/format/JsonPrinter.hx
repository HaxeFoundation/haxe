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

package haxe.format;

class JsonPrinter {
	static public function print(o:Dynamic, ?replacer:(key:Dynamic, value:Dynamic) -> Dynamic, ?space:String):String {
		var printer = new JsonPrinter(replacer, space);
		printer.write("", o);
		return printer.buf.toString();
	}

	var buf:StringBuf;
	var replacer:(key:Dynamic, value:Dynamic) -> Dynamic;
	var indent:String;
	var pretty:Bool;
	var nind:Int;

	function new(replacer:(key:Dynamic, value:Dynamic) -> Dynamic, space:String) {
		this.replacer = replacer;
		this.indent = space;
		this.pretty = space != null;
		this.nind = 0;

		buf = new StringBuf();
	}

	inline function ipad():Void {
		if (pretty)
			add(StringTools.lpad('', indent, nind * indent.length));
	}

	inline function newl():Void {
		if (pretty)
			addChar('\n'.code);
	}

	function write(k:Dynamic, v:Dynamic) {
		if (replacer != null)
			v = replacer(k, v);
		switch (Type.typeof(v)) {
			case TUnknown:
				add('"???"');
			case TObject:
				objString(v);
			case TInt:
				add(v);
			case TFloat:
				add(Math.isFinite(v) ? Std.string(v) : 'null');
			case TFunction:
				add('"<fun>"');
			case TClass(c):
				if (c == String)
					quote(v);
				else if (c == Array) {
					var v:Array<Dynamic> = v;
					addChar('['.code);

					var len = v.length;
					var last = len - 1;
					for (i in 0...len) {
						if (i > 0)
							addChar(','.code)
						else
							nind++;
						newl();
						ipad();
						write(i, v[i]);
						if (i == last) {
							nind--;
							newl();
							ipad();
						}
					}
					addChar(']'.code);
				} else if (c == haxe.ds.StringMap) {
					var v:haxe.ds.StringMap<Dynamic> = v;
					var o = {};
					for (k in v.keys())
						Reflect.setField(o, k, v.get(k));
					objString(o);
				} else if (c == Date) {
					var v:Date = v;
					quote(v.toString());
				} else
					classString(v);
			case TEnum(_):
				var i:Dynamic = Type.enumIndex(v);
				add(i);
			case TBool:
				add(v);
			case TNull:
				add('null');
		}
	}

	extern inline function addChar(c:Int) {
		buf.addChar(c);
	}

	extern inline function add(v:String) {
		buf.add(v);
	}

	function classString(v:Dynamic) {
		fieldsString(v, Type.getInstanceFields(Type.getClass(v)));
	}

	inline function objString(v:Dynamic) {
		fieldsString(v, Reflect.fields(v));
	}

	function fieldsString(v:Dynamic, fields:Array<String>) {
		addChar('{'.code);
		var len = fields.length;
		var last = len - 1;
		var first = true;
		for (i in 0...len) {
			var f = fields[i];
			var value = Reflect.field(v, f);
			if (Reflect.isFunction(value))
				continue;
			if (first) {
				nind++;
				first = false;
			} else
				addChar(','.code);
			newl();
			ipad();
			quote(f);
			addChar(':'.code);
			if (pretty)
				addChar(' '.code);
			write(f, value);
			if (i == last) {
				nind--;
				newl();
				ipad();
			}
		}
		addChar('}'.code);
	}

	function quote(s:String) {
		if (s.length != neko.Utf8.length(s)) {
			quoteUtf8(s);
			return;
		}
		addChar('"'.code);
		var i = 0;
		while (true) {
			var c = StringTools.fastCodeAt(s, i++);
			if (StringTools.isEof(c))
				break;
			switch (c) {
				case '"'.code:
					add('\\"');
				case '\\'.code:
					add('\\\\');
				case '\n'.code:
					add('\\n');
				case '\r'.code:
					add('\\r');
				case '\t'.code:
					add('\\t');
				case 8:
					add('\\b');
				case 12:
					add('\\f');
				default:
					addChar(c);
			}
		}
		addChar('"'.code);
	}

	function quoteUtf8(s:String) {
		var u = new neko.Utf8();
		neko.Utf8.iter(s, function(c) {
			switch (c) {
				case '\\'.code, '"'.code:
					u.addChar('\\'.code);
					u.addChar(c);
				case '\n'.code:
					u.addChar('\\'.code);
					u.addChar('n'.code);
				case '\r'.code:
					u.addChar('\\'.code);
					u.addChar('r'.code);
				case '\t'.code:
					u.addChar('\\'.code);
					u.addChar('t'.code);
				case 8:
					u.addChar('\\'.code);
					u.addChar('b'.code);
				case 12:
					u.addChar('\\'.code);
					u.addChar('f'.code);
				default:
					u.addChar(c);
			}
		});
		buf.add('"');
		buf.add(u.toString());
		buf.add('"');
	}
}
