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
package haxe.ds;

import flash.utils.Dictionary;

@:coreApi class StringMap<V> implements haxe.Constraints.IMap<String,V> {
	var d:Dictionary;
	
	public function new() : Void {
		d = new Dictionary(false);
	}

	public function set( key : String, value : V ) : Void {
		untyped d["$"+key] = value;
	}

	public function get( key : String ) : Null<V> {
		return untyped d["$"+key];
	}

	public inline function exists( key : String ) : Bool {
		return untyped __in__("$"+key,d);
	}

	public function remove( key : String ) : Bool {
		key = "$"+key;
		if( !exists(key) ) return false;
		untyped __delete__(d,key);
		return true;
	}
	
	#if as3

 	public function keys() : StringMapKeysIterator<String, V> {
		var array:Array<String> = untyped __keys__(d);
		return new StringMapKeysIterator<String, V>(array, 0, array.length);
 	}

 	public function iterator() : StringMapValuesIterator<String, V> {
		var ret:Array<V> = [];
		for (i in keys())
			ret.push(get(i));
		return new StringMapValuesIterator<String, V>(ret, 0, ret.length);
 	}
	
	#else

	public inline function keys() : StringMapKeysIterator<String, V> {
		return new StringMapKeysIterator<String, V>(d, 0);
	}

	public inline function iterator() : StringMapValuesIterator<String, V> {
		return new StringMapValuesIterator<String, V>(d, 0);
	}

	#end
	
	public function toString() : String {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for( i in it ) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

}

#if as3
class StringMapKeysIterator<K, V> {
	var collection:Array<K>;
	var index:Int;
	var len:Int;
	public inline function new(c:Array<K>, i:Int, l:Int) {
		this.collection = c;
		index = i;
		len = l;
	}
	public inline function hasNext():Bool {
		return index < len;
	}
	public inline function next():K {
		return collection[index++];
	}
}
class StringMapValuesIterator<K, V> {
	var collection:Array<V>;
	var index:Int;
	var len:Int;
	public inline function new(c:Array<V>, i:Int, l:Int) {
		this.collection = c;
		index = i;
		len = l;
	}
	public inline function hasNext():Bool {
		return index < len;
	}
	public inline function next():V {
		return collection[index++];
	}
}
#else
class StringMapKeysIterator<K, V> {
	var collection:Dictionary;
	var index:Int;
	public inline function new(c:Dictionary, i:Int) {
		this.collection = c;
		index = i;
	}
	public inline function hasNext():Bool {
		var c = collection;
		var i = index;
		var result = untyped __has_next__(c, i);
		collection = c;
		index = i;
		return result;
	}
	public inline function next():K {
		return untyped __forin__(collection, index);
	}
}

class StringMapValuesIterator<K, V> {
	var collection:Dictionary;
	var index:Int;
	public inline function new(c:Dictionary, i:Int) {
		this.collection = c;
		index = i;
	}
	public inline function hasNext():Bool {
		var c = collection;
		var i = index;
		var result = untyped __has_next__(c, i);
		collection = c;
		index = i;
		return result;
	}
	public inline function next():V {
		return untyped __foreach__(collection, index);
	}
}
#end
