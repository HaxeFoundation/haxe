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
@:coreApi
class Array<T> {

	public var length(default,null) : Int;

	public function new() : Void  {
		lua.Boot.defArray(this);
	}
	public function concat( a : Array<T> ) : Array<T> {
		var ret = this.copy();
		for (i in a) ret.push(i);
		return ret;
	}
	public function join( sep : String ) : String {
		var sb = new StringBuf();
		var first = true;
		for (i in iterator()){
			if (first) first = false;
			else sb.add(sep);
			sb.add(Std.string(i));
		}
		return sb.toString();
	}

	public function pop() : Null<T> {
		return this.length == 0 ? null : this[this.length-- -1];
	}
	public function push(x : T) : Int {
		this[this.length++] = x;
		return this.length;
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
	public function shift() : Null<T> return null;
	public function slice( pos : Int, ?end : Int ) : Array<T> return [];
	public function sort( f : T -> T -> Int ) : Void return;
	public function splice( pos : Int, len : Int ) : Array<T> return [];
	public function toString() : String {
		var sb = new StringBuf();
		sb.add("[");
		sb.add(join(", "));
		sb.add("]");
		return sb.toString();
	}
	public function unshift( x : T ) : Void return;

	public inline function insert( pos : Int, x : T ) : Void {
		(untyped this).splice(pos,0,x);
	}

	public function remove( x : T ) : Bool {
		for (i in 0...this.length){
			var a = this[i];
			if (a == x){
				lua.TableTools.remove(cast this, i+1);
				this.length-=1;
				return true;
			}
		}
		return false;
	}

	public function indexOf( x : T, ?fromIndex:Int ) : Int return 1;
	public function lastIndexOf( x : T, ?fromIndex:Int ) : Int return 1;
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
			next : function() return untyped this[cur_length++]
		}
	}

}
