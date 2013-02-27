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
@:coreApi class EReg {

	var r : Dynamic;

	public function new( r : String, opt : String ) : Void {
		opt = opt.split("u").join(""); // 'u' (utf8) depends on page encoding
		this.r = untyped __new__("RegExp",r,opt);
	}

	public function match( s : String ) : Bool {
		if( r.global ) r.lastIndex = 0;
		r.m = r.exec(s);
		r.s = s;
		return (r.m != null);
	}

	public function matched( n : Int ) : String {
		return if( r.m != null && n >= 0 && n < r.m.length ) r.m[n] else throw "EReg::matched";
	}

	public function matchedLeft() : String {
		if( r.m == null ) throw "No string matched";
		return r.s.substr(0,r.m.index);
	}

	public function matchedRight() : String {
		if( r.m == null ) throw "No string matched";
		var sz = r.m.index+r.m[0].length;
		return r.s.substr(sz,r.s.length-sz);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		if( r.m == null ) throw "No string matched";
		return { pos : r.m.index, len : r.m[0].length };
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		return if (r.global) {
			r.lastIndex = pos;
			r.m = r.exec(len < 0 ? s : s.substr(0, pos + len));
			var b = r.m != null;
			if (b) {
				r.s = s;
			}
			b;
		} else {
			// TODO: check some ^/$ related corner cases
			var b = match( len < 0 ? s.substr(pos) : s.substr(pos,len) );
			if (b) {
				r.s = s;
				r.m.index += pos;
			}
			b;
		}
	}

	public function split( s : String ) : Array<String> {
		// we can't use directly s.split because it's ignoring the 'g' flag
		var d = "#__delim__#";
		return untyped s.replace(r,d).split(d);
	}

	public function replace( s : String, by : String ) : String {
		return untyped s.replace(r,by);
	}

	public function map( s : String, f : EReg -> String ) : String {
		var offset = 0;
		var buf = new StringBuf();
		do {
			if (offset >= s.length)
				break;
			else if (!matchSub(s, offset)) {
				buf.add(s.substr(offset));
				break;
			}
			var p = matchedPos();
			buf.add(s.substr(offset, p.pos - offset));
			buf.add(f(this));
			if (p.len == 0) {
				buf.add(s.substr(p.pos, 1));
				offset = p.pos + 1;
			}
			else
				offset = p.pos + p.len;
		} while (r.global);
		if (!r.global && offset > 0 && offset < s.length)
			buf.add(s.substr(offset));
		return buf.toString();
	}
}
