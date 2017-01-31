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
package haxe.ds;

import php.Syntax;
import php.Global;
import php.NativeArray;
import php.NativeAssocArray;
import haxe.Constraints;

@:coreApi class StringMap<T> implements IMap<String,T> {
	private var data : NativeAssocArray<T>;

	public inline function new() : Void {
		data = new NativeAssocArray();
	}

	public inline function set( key : String, value : T ) : Void {
		data[key] = value;
	}

	public inline function get( key : String ) : Null<T> {
		return Syntax.binop(data[key], '??', null);
	}

	public inline function exists( key : String ) : Bool {
		return Global.array_key_exists(key, data);
	}

	public function remove( key : String ) : Bool {
		if (Global.array_key_exists(key, data)) {
			Global.unset(data[key]);
			return true;
		} else {
			return false;
		}
	}

	public inline function keys() : Iterator<String> {
		return Global.array_map('strval', Global.array_keys(data)).iterator();
	}

	public inline function iterator() : Iterator<T> {
		return data.iterator();
	}

	public function toString() : String {
		var parts = new NativeArray();
		Syntax.foreach(data, function(key:String, value:T) {
			Global.array_push(parts, '$key => ' + Std.string(value));
		});

		return '{' + Global.implode(', ', parts) + '}';
	}
}
