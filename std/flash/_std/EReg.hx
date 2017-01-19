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
@:coreApi class EReg {

	var r : flash.utils.RegExp;
	var result : Dynamic;

	public function new( r : String, opt : String ) : Void {
		this.r = new flash.utils.RegExp(r,opt);
	}

	public function match( s : String ) : Bool {
		if( r.global ) r.lastIndex = 0;
		result = r.exec(s);
		return (result != null);
	}

	public function matched( n : Int ) : String {
		return if( result != null && n >= 0 && n < (result : Array<Dynamic>).length ) result[n] else throw "EReg::matched";
	}

	public function matchedLeft() : String {
		if( result == null ) throw "No string matched";
		var s : String = untyped result.input;
		return s.substr(0,untyped result["index"]);
	}

	public function matchedRight() : String {
		if( result == null ) throw "No string matched";
		var rl = (result[untyped "index"] : Int) + (result[0] : String).length;
		var s : String = untyped result.input;
		return s.substr(rl,s.length - rl);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		if( result == null ) throw "No string matched";
		return { pos : result[untyped "index"], len : (result[0] : String).length };
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		return if (r.global) {
			r.lastIndex = pos;
			result = r.exec(len < 0 ? s : s.substr(0, pos + len));
			var b = result != null;
			if (b) {
				untyped result.input = s;
			}
			b;
		} else {
			var b = match( len < 0 ? s.substr(pos) : s.substr(pos,len) );
			if (b) {
				untyped result.input = s;
				untyped result["index"] += pos;
			}
			b;
		}
	}

	public function split( s : String ) : Array<String> {
		// we can't use directly s.split because it's ignoring the 'g' flag
		var d = "#__delim__#";
		var s : String = untyped s.replace(r, d);
		return s.split(d);
	}

	public function replace( s : String, by : String ) : String {
		return untyped s.replace(r,by);
	}

	public function map( s : String, f : EReg -> String ) : String {
		var offset = 0;
		var buf = new StringBuf();
		var first = true;
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
			first = false;
		} while (r.global);
		if (!r.global && offset > 0 && offset < s.length)
			buf.add(s.substr(offset));
		return buf.toString();
	}
}
