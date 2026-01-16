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

package cs;

import haxe.ds.StringMap;

@:keep
@:native('haxe.root.HaxeDynamicObject')
@:nativeGen
class HaxeDynamicObject extends HaxeObject {
	private static var __hx_toString_depth:Int = 0;

	private var _hx_fields:Null<StringMap<Dynamic>>;

	public function new() {
		super();
		_hx_fields = new StringMap();
	}

	// Factory method to create with initial field values
	// Usage: _hx_create(["field1", value1, "field2", value2, ...])
	public static function _hx_create(args:Array<Dynamic>):HaxeDynamicObject {
		var obj = new HaxeDynamicObject();
		var i = 0;
		while (i < args.length) {
			var name:String = args[i];
			var value:Dynamic = args[i + 1];
			obj._hx_setField(name, value);
			i += 2;
		}
		return obj;
	}

	public function toString():String {
		if (__hx_toString_depth >= 5) {
			return "...";
		}
		++__hx_toString_depth;
		_hx_initFields();
		if (_hx_hasField("toString")) {
			--__hx_toString_depth;
			return _hx_getField("toString")();
		}
		var buf = new StringBuf();
		buf.addChar("{".code);
		var first = true;
		try {
			for (key in _hx_fields.keys()) {
				if (first)
					first = false
				else
					buf.add(", ");
				buf.add(key);
				buf.add(": ");
				buf.add(_hx_fields.get(key));
			}
		} catch (e:Dynamic) {
			--__hx_toString_depth;
			throw(e);
		}
		--__hx_toString_depth;
		buf.addChar("}".code);
		return buf.toString();
	}

	override public function _hx_deleteField(name:String):Bool {
		_hx_initFields();
		return _hx_fields.remove(name);
	}

	override public function _hx_getFields():Array<String> {
		_hx_initFields();
		return [for (key in _hx_fields.keys()) key];
	}

	override public function _hx_getField(name:String):Dynamic {
		_hx_initFields();
		return _hx_fields.get(name);
	}

	public function _hx_hasField(name:String):Bool {
		_hx_initFields();
		return _hx_fields.exists(name);
	}

	override public function _hx_setField(name:String, value:Dynamic):Void {
		_hx_initFields();
		_hx_fields.set(name, value);
	}

	private function _hx_initFields():Void {
		if (_hx_fields == null) {
			_hx_fields = new StringMap();
		}
	}
}
