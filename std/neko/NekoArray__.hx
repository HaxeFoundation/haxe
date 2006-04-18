/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package neko;

class NekoArray__<T> implements Array<T> {

	public property length(default,null) : Int;

	private function new() {
		untyped {
			this.__a = __dollar__amake(0);
			this.length = 0;
		}
	}

	private static function new1(a,l) {
		untyped {
			if( __dollar__typeof(a) != __dollar__tarray )
				__dollar__throw(a);
			var inst = new NekoArray__<Dynamic>();
			inst.__a = a;
			inst.length = l;
			return inst;
		}
	}

	public function concat(arr) {
		untyped {
			var a1 = this.__a;
			var a2 = arr.__a;
			var s1 = this.length;
			var s2 = arr.length;
			var a = __dollar__amake(s1+s2);
			__dollar__ablit(a,0,a1,0,s1);
			__dollar__ablit(a,s1,a2,0,s2);
			return Array.new1(a,s1+s2);
		}
	}

	public function copy() {
		return untyped Array.new1(__dollar__acopy(this.__a),this.length);
	}

	public function indexes() {
		return untyped {
			a : this.__a,
			p : 0,
			l : this.length,
			hasNext : function() {
				while( this.p < this.l ) {
					if( this.a[this.p] != null )
						return true;
					this.p += 1;
				}
				return false;
			},
			next : function() {
				var i = this.p;
				this.p += 1;
				return i;
			}
		};
	}

	public function iterator() {
		return untyped {
			a : this.__a,
			p : 0,
			l : this.length,
			hasNext : function() {
				while( this.p < this.l ) {
					if( this.a[this.p] != null )
						return true;
					this.p += 1;
				}
				return false;
			},
			next : function() {
				var i = this.a[this.p];
				this.p += 1;
				return i;
			}
		};
	}

	public function insert( pos, x ) {
		untyped {
			this.__resize(if( length < pos ) pos + 1 else length + 1);
			this.__a[pos] = x;
		}
	}

	public function join(delim : String) {
		var s = new StringBuf();
		var it = iterator();
		for( i in it ) {
			s.add(i);
			if( it.hasNext() )
				s.add(delim);
		}
		return s.toString();
	}

	public function toString() {
		var s = new StringBuf();
		s.add("[");
		var it = iterator();
		for( i in it ) {
			s.add(i);
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("]");
		return s.toString();
	}

	public function pop() {
		untyped {
			this.length -= 1;
			var x = this.__a[this.length];
			this.__a[this.length] = null;
			return x;
		}
	}

	public function push(v) {
		untyped {
			var l = this.length;
			this.__resize(l + 1);
			this.__a[l] = v;
			return l;
		}
	}

	public function unshift(v ) {
		untyped {
			var l = this.length;
			this.__resize(l + 1);
			var a = this.__a;
			__dollar__ablit(a,1,a,0,l);
			a[0] = v;
		}
	}

	public function remove(v) {
		untyped {
			var i = 0;
			var l = this.length;
			var a = this.__a;
			while( i < l ) {
				if( a[i] == v ) {
					__dollar__ablit(a,i,a,i+1,l - i - 1);
					l -= 1;
					this.length = l;
					a[l] = null;
					return true;
				}
				i += 1;
			}
			return false;
		}
	}

	public function reverse() {
		untyped {
			var i = 0;
			var l = this.length;
			var a = __dollar__asub(this.__a,0,l);
			var half = __dollar__int(l / 2);
			l -= 1;
			while( i < half ) {
				var tmp = a[i];
				a[i] = a[l-i];
				a[l-i] = tmp;
				i += 1;
			}
			return Array.new1(a,l+1);
		}
	}

	public function shift() {
		untyped {
			var l = this.length;
			if( l == 0 )
				return null;
			var a = this.__a;
			var x = a[0];
			l -= 1;
			__dollar__ablit(a,0,a,1,l);
			a[l] = null;
			this.length = l;
			return x;
		}
	}

	public function slice( pos, end ) {
		var len = end - pos;
		return untyped Array.new1(__dollar__asub(this.__a,pos,len),len);
	}

	public function sort(f) {
		untyped {
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
	}

	public function splice( pos, len ) {
		untyped {
			var a = this.__a;
			var ret = Array.new1(__dollar__asub(a,pos,len),len);
			var end = pos + len;
			__dollar__ablit(a,pos,a,end,this.length-end);
			this.length -= len;
			return ret;
		}
	}



	/* NEKO INTERNAL */

	private function __get( pos  ) {
		return untyped this.__a[pos];
	}

	private function __set( pos, v ) {
		untyped {
			var a = this.__a;
			if( __dollar__asize(a) <= pos ) {
				this.__resize(pos+1);
				a = this.__a;
			}
			a[pos] = v;
		}
	}

	private function __resize(l) {
		untyped {
			var a = this.__a;
			if( __dollar__asize(a) >= l ) {
				this.length = l;
				return;
			}
			var a2 = __dollar__amake(l);
			__dollar__ablit(a2,0,a,0,this.length);
			this.__a = a2;
			this.length = l;
		}
	}


}
