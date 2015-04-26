/*
 * Copyright (C)2005-2015 Haxe Foundation
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
import lua.Rex;
import lua.Table;
import lua.Boot;
import lua.TableTools;
// @:coreApi
class EReg {

	var r : Rex; // the Rex extern instance.
	var global : Bool;  // whether the regex is in global mode.
	var s : String; // the last matched string
	var m : Table<Int,Dynamic>; // the [start:Int, end:Int, and submatches:String (matched groups)] as a single table.

	public function new( r : String, opt : String ) : Void {
		var ropt = new StringBuf();
		for (i in 0...opt.length){
			switch(opt.charAt(i)){
				case "i", "m", "s" : ropt.add(opt.charAt(i));
				case "g" : global = true;
				default : null;
			}
		}
		if (global == null) global = false;
		this.r = Rex.create(r, ropt.toString());
	}

	public function match( str : String ) : Bool {
		m = untyped Boot.unpack(r.exec(str));
		s = str;
		return m[0] != null;
	}

	public function matched( n : Int ) : String {
		if (m == null || n < 0) throw "EReg::matched";
		else if (n == 0) {
			// TODO: Figure out how to use lua 1-based indexing where appropriate,
			// 	while also providing the lua.Table utility abstract.
			return untyped __lua__("string.sub(self.s, self.m[1], self.m[2])");
		} else {
			var mn = 2 * (n - 1);
			return untyped __lua__("string.sub(self.s, self.m[3][mn + 1], self.m[3][mn + 2])");
		}
	}

	public function matchedLeft() : String {
		if( m == null ) throw "No string matched";
		return untyped __lua__("string.sub(self.s, 1, self.m[1]-1)");
	}

	public function matchedRight() : String {
		if( m == null ) throw "No string matched";
		return untyped __lua__("string.sub(self.s, self.m[2]+1)");
	}

	public function matchedPos() : { pos : Int, len : Int } {
		if( m == null ) throw "No string matched";
		return {
			pos : m[0]-1,
			len : m[1]- m[0]+ 1
		}
	}

	public inline function matchSub( s : String, pos : Int, ?len : Int):Bool {
		return match(s.substr(pos, len));
	}

	public function split( s : String ) : Array<String> {
		// we can't use directly s.split because it's ignoring the 'g' flag
		var d = "#__delim__#";
		return untyped s.replace(r,d).split(d);
	}

	public function replace( s : String, by : String ) : String {
		if (global){
			return split(s).join(by);
		} else {
			if (match(s)) return matchedLeft() + by + matchedRight();
			else return s;
		}
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
		} while (global);
		if (!global && offset > 0 && offset < s.length)
			buf.add(s.substr(offset));
		return buf.toString();
	}

}

