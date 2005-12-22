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

class NekoString__ implements String {

	private static var __split : Dynamic = Neko.load("std","string_split",2);

	public var length : Int;

	private function new(s) {
		untyped {
			if( __dollar__typeof(s) != __dollar__tstring )
				s = __dollar__string(s);
			this.__s = s;
			this.length = __dollar__ssize(s);
		}
	}

	public function charAt(p) {
		untyped {
			var s = __dollar__smake(1);
			__dollar__sset(s,0,__dollar__sget(this.__s,p));
			return new String(s);
		}
	}

	public function charCodeAt(p) {
		untyped {
			return __dollar__sget(this.__s,p);
		}
	}

	public function indexOf( str, pos ) {
		untyped {
			var p = __dollar__sfind(this.__s,pos,str.__s);
			if( p == null )
				return -1;
			return p;
		}
	}

	public function lastIndexOf( str, pos ) {
		untyped {
			var last = -1;
			while( true ) {
				var p = __dollar__sfind(this.__s,last + 1,str.__s);
				if( p == null )
					return last;
				last = p;
			}
			return null;
		}
	}

	public function split( delim ) {
		untyped {
			var l = __split(this.__s,delim.__s);
			var a = new Array<String>();
			while( l != null ) {
				a.push(new String(l[0]));
				l = l[1];
			}
			return a;
		}
	}

	public function substr( pos, len ) {
		return untyped new String(__dollar__ssub(this.__s,pos,len));
	}

	public function toLowerCase() {
		untyped {
			var s = this.__s;
			var l = this.length;
			var s2 = __dollar__scopy(s);
			var i = 0;
			while( i < l ) {
				var c = __dollar__sget(s,i);
				if( c >= 65 && c <= 90 )
					__dollar__sset(s2,i,c-65+97);
				i++;
			}
			return new String(s2);
		}
	}

	public function toUpperCase() {
		untyped {
			var s = this.__s;
			var l = this.length;
			var s2 = __dollar__scopy(s);
			var i = 0;
			while( i < l ) {
				var c = __dollar__sget(s,i);
				if( c >= 97 && c <= 122 )
					__dollar__sset(s2,i,c-97+65);
				i++;
			}
			return new String(s2);
		}
	}

	public function toString() {
		return this;
	}

	/* NEKO INTERNALS */

	private function __compare(o) {
		return untyped __compare__(this.__s,o.__s);
	}

	private function __add(s) {
		return untyped new String(this.__s+__dollar__string(s));
	}

	private function __radd(s) {
		return untyped new String(__dollar__string(s)+this.__s);
	}

}
