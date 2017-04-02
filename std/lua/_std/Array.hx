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
@:coreApi
class Array<T> {

	public var length(default,null) : Int;

	public function new() : Void  {
		untyped _hx_tab_array(this, 0);
	}
	public function concat( a : Array<T> ) : Array<T> {
		var ret = this.copy();
		for (i in a) ret.push(i);
		return ret;
	}
	public function join( sep : String ) : String {
		var tbl : lua.Table<Int,String> = lua.Table.create();
		for (i in iterator()){
			lua.Table.insert(tbl,Std.string(i));
		}
		return lua.Table.concat(tbl,sep);
	}

	public function pop() : Null<T> {
		if (length == 0 ) return null;
		var rawlength = lua.Lua.rawget(cast this, 'length');
		var ret = lua.Lua.rawget(cast this, rawlength-1);
		lua.Lua.rawset(cast this, 'length', rawlength-1);
		return ret;
	}
	public function push(x : T) : Int {
		lua.Lua.rawset(cast this,length,x);
		lua.Lua.rawset(cast this,'length', length + 1);
		return lua.Lua.rawget(cast this,'length');
	}
	public function reverse() : Void {
		var tmp:T;
		var i = 0;
		while(i < Std.int(this.length/2)){
			tmp = this[i];
			this[i] = this[this.length-i-1];
			this[this.length-i-1] = tmp;
			i++;
		}
	}
	public function shift() : Null<T> {
		if (this.length == 0) return null;
		var ret = this[0];
		for (i in 0...length){
			this[i] = this[i+1];
		}
		this.length-=1;
		return ret;
	}
	public function slice( pos : Int, ?end : Int ) : Array<T> {
		if (end == null || end > length) end = length;
		else if (end < 0) end = (length-(-end % length)) % length; // negative pos needs to be wrapped from the end, and mod according to array length
		if (pos < 0) pos = (length -(-pos % length)) % length;  // and here
		if (pos > end || pos > length) return [];

		var ret = [];
		for (i in pos...end){
			ret.push(this[i]);
		}
		return ret;
	}

	// TODO: copied from neko Array.sort, move to general util library?
	public function sort( f : T -> T -> Int ) : Void {
		var i = 0;
		var l = this.length;
		while( i < l ) {
			var swap = false;
			var j = 0;
			var max = l - i - 1;
			while( j < max ) {
				if( f(this[j],this[j+1]) > 0 ) {
					var tmp = this[j+1];
					this[j+1] = this[j];
					this[j] = tmp;
					swap = true;
				}
				j += 1;
			}
			if( !swap )
				break;
			i += 1;
		}
	}

	public function splice( pos : Int, len : Int ) : Array<T> {
		if (len < 0 || pos > length) return [];
		else if (pos < 0) pos = length -(-pos % length);
		len = cast Math.min(len,this.length-pos);
		var ret = [];
		for (i in pos...(pos+len)){
			ret.push(this[i]);
			this[i] = this[i+len];
		}
		for (i in (pos+len)...length){
			this[i] = this[i+len];
		}
		this.length-= len;
		return ret;
	}

	public function toString() : String {
		var tbl : lua.Table<Int,String> = lua.Table.create();
		lua.Table.insert(tbl, '[');
		lua.Table.insert(tbl, join(","));
		lua.Table.insert(tbl, ']');
		return lua.Table.concat(tbl,"");
	}

	public function unshift( x : T ) : Void {
		var len = length;
		for (i in 0...len) this[len - i] = this[len - i - 1];
		this[0] = x;
	}

	public inline function insert( pos : Int, x : T ) : Void {
		if (pos > length) pos = length;
		if (pos < 0) {
			pos = (length + pos);
			if (pos < 0) pos = 0;
		}
		var cur_len = length;
		while (cur_len > pos){
			this[cur_len] = this[cur_len-1];
			cur_len -=1;
		}
		this[pos] = x;
	}

	public function remove( x : T ) : Bool {
		for (i in 0...length){
			if (this[i] == x){
				for (j in i...length-1){
					this[j] = this[j+1];
				}
				// We need to decrement the length variable, and set its
				// value to null to avoid hanging on to a reference in the
				// underlying lua table.
				this[length-1] = null;
				// Do this in two steps to avoid re-updating the __index metamethod
				length--;

				return true;
			}
		}
		return false;
	}

	public function indexOf( x : T, ?fromIndex:Int ) : Int {
		var end = length;
		if (fromIndex == null) fromIndex = 0;
		else if (fromIndex < 0 ) {
			fromIndex = length + fromIndex;
			if (fromIndex < 0) fromIndex = 0;
		}
		for (i in fromIndex...end){
			if (x == this[i]) return i;
		}
		return -1;
	}
	public function lastIndexOf( x : T, ?fromIndex:Int ) : Int {
		if (fromIndex == null || fromIndex >= length ) fromIndex = length-1;
		else if (fromIndex < 0) {
			fromIndex = length + fromIndex;
			if (fromIndex < 0) return -1;
		}
		var i = fromIndex;
		while(i >= 0){
			if (this[i] == x) return i;
			else i--;
		}
		return -1;
	}
	public inline function copy() : Array<T> {
		return [for (i in this) i];
	}
	public function map<S>(f:T->S):Array<S> {
		return [for (i in this) f(i)];
	}
	public function filter(f:T->Bool):Array<T> {
		return [for (i in this) if (f(i)) i];
	}
	public inline function iterator() : Iterator<T> {
		var cur_length = 0;
		return {
			hasNext : function() return cur_length < length,
			next : function() return this[cur_length++]
		}
	}
	private static function __init__() : Void{
		// table-to-array helper
		haxe.macro.Compiler.includeFile("lua/_lua/_hx_tab_array.lua");
	}

}
