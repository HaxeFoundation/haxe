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
@:coreApi @:final class Array<T> {

	private var __a : neko.NativeArray<T>;
	public var length(default,null) : Int;

	public function new() : Void {
		this.__a = neko.NativeArray.alloc(0);
		this.length = 0;
	}

	private static function new1<T>(a:neko.NativeArray<T>,l:Int) : Array<T> {
		var inst = new Array<T>();
		inst.__a = a;
		inst.length = l;
		return inst;
	}

	public function concat( a : Array<T>) : Array<T> {
		var a1 = this.__a;
		var a2 = a.__a;
		var s1 = this.length;
		var s2 = a.length;
		var a = neko.NativeArray.alloc(s1+s2);
		neko.NativeArray.blit(a,0,a1,0,s1);
		neko.NativeArray.blit(a,s1,a2,0,s2);
		return new1(a,s1+s2);
	}

	public function copy() : Array<T> {
		return new1(neko.NativeArray.sub(this.__a,0,this.length),this.length);
	}

	public function iterator() : Iterator<T> {
		return untyped {
			a : this,
			p : 0,
			hasNext : function() {
				return __this__.p < __this__.a.length;
			},
			next : function() {
				var i = __this__.a.__a[__this__.p];
				__this__.p += 1;
				return i;
			}
		};
	}

	public function insert( pos : Int, x : T ) : Void {
		var l = this.length;
		if( pos < 0 ) {
			pos = l + pos;
			if( pos < 0 ) pos = 0;
		}
		if( pos > l ) pos = l;
		this.__grow(l+1);
		var a = this.__a;
		neko.NativeArray.blit(a,pos+1,a,pos,l-pos);
		a[pos] = x;
	}

	public function join( sep : String ) : String {
		var s = new StringBuf();
		var a = this.__a;
		var max = this.length - 1;
		for( p in 0...this.length ) {
			s.add(a[p]);
			if( p != max )
				s.add(sep);
		}
		return s.toString();
	}

	public function toString() : String {
		var s = new StringBuf();
		s.add("[");
		var it = iterator();
		for( i in it ) {
			s.add(i);
			if( it.hasNext() )
				s.addChar(",".code);
		}
		s.add("]");
		return s.toString();
	}

	public function pop() : Null<T> {
		if( this.length == 0 )
			return null;
		this.length -= 1;
		var x = this.__a[this.length];
		this.__a[this.length] = null;
		return x;
	}

	public function push(x:T) : Int {
		var l = this.length;
		this.__grow(l + 1);
		this.__a[l] = x;
		return l + 1;
	}

	public function unshift(x : T) : Void {
		var l = this.length;
		this.__grow(l + 1);
		var a = this.__a;
		neko.NativeArray.blit(a,1,a,0,l);
		a[0] = x;
	}

	public function remove(x : T) : Bool {
		var i = 0;
		var l = this.length;
		var a = this.__a;
		while( i < l ) {
			if( a[i] == x ) {
				neko.NativeArray.blit(a,i,a,i+1,l - i - 1);
				l -= 1;
				this.length = l;
				a[l] = null;
				return true;
			}
			i += 1;
		}
		return false;
	}

	public function indexOf(x : T, ?fromIndex:Int) : Int {
		var len = length;
		var i:Int = (fromIndex != null) ? fromIndex : 0;
		var a = __a;
		if (i < 0) {
			i += len;
			if (i < 0) i = 0;
		}
		while (i < len)
		{
			if (a[i] == x)
				return i;
			i++;
		}
		return -1;
	}

	public function lastIndexOf(x : T, ?fromIndex:Int) : Int {
		var len = length;
		var i:Int = (fromIndex != null) ? fromIndex : len - 1;
		var a = __a;
		if (i >= len)
			i = len - 1;
		else if (i < 0)
			i += len;
		while (i >= 0)
		{
			if (a[i] == x)
				return i;
			i--;
		}
		return -1;
	}

	public function reverse() : Void {
		var i = 0;
		var l = this.length;
		var a = this.__a;
		var half = l >> 1;
		l -= 1;
		while( i < half ) {
			var tmp = a[i];
			a[i] = a[l-i];
			a[l-i] = tmp;
			i += 1;
		}
	}

	public function shift() : Null<T> {
		var l = this.length;
		if( l == 0 )
			return null;
		var a = this.__a;
		var x = a[0];
		l -= 1;
		neko.NativeArray.blit(a,0,a,1,l);
		a[l] = null;
		this.length = l;
		return x;
	}

	public function slice( pos : Int, ?end : Int ) : Array<T> {
		if( pos < 0 ){
			pos = this.length + pos;
			if( pos < 0 )
				pos = 0;
		}
		if( end == null )
			end = this.length;
		else if( end < 0 )
			end = this.length + end;
		if( end > this.length )
			end = this.length;
		var len = end - pos;
		if( len < 0 ) return new Array();
		return new1(neko.NativeArray.sub(this.__a,pos,len),len);
	}

	public function sort(f:T->T->Int) : Void {
		var a = this.__a;
		var i = 0;
		var l = this.length;
		while( i < l ) {
			var swap = false;
			var j = 0;
			var max = l - i - 1;
			while( j < max ) {
				if( f(a[j],a[j+1]) > 0 ) {
					var tmp = a[j+1];
					a[j+1] = a[j];
					a[j] = tmp;
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
		if( len < 0 ) return new Array();
		if( pos < 0 ){
			pos = this.length + pos;
			if( pos < 0 ) pos = 0;
		}
		if( pos > this.length ) {
			pos = 0;
			len = 0;
		} else if( pos + len > this.length ) {
			len = this.length - pos;
			if( len < 0 ) len = 0;
		}
		var a = this.__a;
		var ret = new1(neko.NativeArray.sub(a,pos,len),len);
		var end = pos + len;
		neko.NativeArray.blit(a,pos,a,end,this.length-end);
		this.length -= len;
		while( --len >= 0 )
			a[this.length + len] = null;
		return ret;
	}

	public function map<S>( f : T -> S ) : Array<S> {
		var ret = [];
		for (elt in this)
			ret.push(f(elt));
		return ret;
	}

	public function filter( f : T -> Bool ) : Array<T> {
		var ret = [];
		for (elt in this)
			if (f(elt))
				ret.push(elt);
		return ret;
	}


	/* NEKO INTERNAL */

	private function __get( pos : Int ) : T {
		return this.__a[pos];
	}

	private function __set( pos : Int, v : T ) : T {
		var a = this.__a;
		if( this.length <= pos ) {
			var l = pos + 1;
			var dlen = l - neko.NativeArray.length(a);
			if( dlen > 0 ) {
				if( dlen == 1 ) {
					this.__grow(l);
					a = this.__a;
				} else {
					a = neko.NativeArray.alloc(l);
					neko.NativeArray.blit(a,0,this.__a,0,this.length);
					this.__a = a;
				}
			}
			this.length = l;
		}
		a[pos] = v;
		return v;
	}

	private function __grow(l:Int) : Void {
		var a = this.__a;
		var sz = neko.NativeArray.length(a);
		if( sz >= l ) {
			this.length = l;
			return;
		}
		var big = (sz * 3) >> 1;
		if( big < l ) big = l;
		var a2 = neko.NativeArray.alloc(big);
		neko.NativeArray.blit(a2,0,a,0,this.length);
		this.__a = a2;
		this.length = l;
	}

	private function __neko() : neko.NativeArray<T> {
		var a = this.__a;
		var sz = neko.NativeArray.length(a);
		if( sz != this.length ) {
			a = neko.NativeArray.sub(a,0,this.length);
			this.__a = a;
		}
		return a;
	}

	#if !(macro || interp)
	static function __init__() : Void {
		try {
			var msort : Dynamic = neko.Lib.load("std","merge_sort",3);
			untyped Array.prototype.sort = function(cmp) msort(__this__.__a,__this__.length,cmp);
		} catch( e : Dynamic ) {
		}
	}
	#end

}
