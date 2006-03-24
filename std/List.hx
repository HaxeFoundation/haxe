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

enum Cell<T> {
	empty;
	cons( item : T, next : Cell<T> );
}

class ListIter<T> implements Iterator<T> {

	private var h : Cell<T>;

	public function new( h : Cell<T> ) {
		this.h = h;
	}

	public function hasNext() : Bool {
		return (h != empty);
	}

	public function next() : T {
		return switch h {
		case empty:
			null;
		case cons(it,h):
			this.h = h;
			it;
		}
	}

}


class List<T> {

	private var h : Cell<T>;

	public function new() {
		h = empty;
	}

	public function push( item : T ) : Void {
		h = cons(item,h);
	}

	public function pop() : T {
		return switch h {
		case empty:
			null;
		case cons(it,h):
			this.h = h;
			it;
		}
	}

	public function isEmpty() : Bool {
		return switch h {
		case empty : true;
		default : false;
		}
	}

	public function remove( v : T ) : Bool {
		var loop;
		var found = { ref : false };
		loop = function(h) {
			return switch h {
			case empty:
				empty;
			case cons(v2,h):
				if( v2 == v ) {
					found.ref = true;
					h;
				} else
					cons(v2,loop(h));
			}
		};
		h = loop(h);
		return found.ref;
	}

	public function length() {
		var c = 0;
		var h = this.h;
		while( true ) {
			switch( h ) {
			case empty: break;
			case cons(_,h2): h = h2; c++;
			}
		}
		return c;
	}

	public function has(x) {
		var h = this.h;
		while( true ) {
			switch( h ) {
			case empty:
				break;
			case cons(x2,h2):
				if( x == x2 )
					return true;
				h = h2;
			}
		}
		return false;
	}

	public function iterator() : Iterator<T> {
		return new ListIter<T>(h);
	}

	public function toString() {
		var s = new StringBuf();
		var it = iterator();
		s.add("{");
		for( i in it ) {
			s.add(i);
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

}
