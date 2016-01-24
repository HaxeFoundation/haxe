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
import lua.lrexlib.Rex;
import lua.Table;
import lua.Boot;

@:coreApi
class EReg {

	var r : Rex; // the Rex extern instance.
	var global : Bool;  // whether the regex is in global mode.
	var s : String; // the last matched string
	var m : Table<Int,Int>; // the [start:Int, end:Int, and submatches:String (matched groups)] as a single table.

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

	public function match( s : String ) : Bool {
		this.m = Table.pack(r.exec(s));
		this.s = s;
		return  m[1] != null;
	}

	public function matched( n : Int ) : String {
		if (m[1] == null || n < 0) throw "EReg::matched";
		else if (n == 0) {
			var k =  lua.StringTools.sub(s, m[1], m[2]);
			return k;
		} else if (Std.is(m[3], lua.Table)){
			var mn = 2 * (n - 1);
			if (Std.is(untyped m[3][mn+1], Bool)) return null;
			return lua.StringTools.sub(s, untyped m[3][mn + 1], untyped m[3][mn + 2]);
		} else {
			throw "EReg:matched";
		}
	}

	public function matchedLeft() : String {
		if( m[1] == null ) throw "No string matched";
		return lua.StringTools.sub(s, 1, m[1]-1);
	}

	public function matchedRight() : String {
		if( m[1] == null ) throw "No string matched";
		return lua.StringTools.sub(s, m[2]+1);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		if( m[1] == null ) throw "No string matched";
		return {
			pos : m[1]-1,
			len : m[2]-m[1]+ 1
		}
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {

		var ss = s.substr(0, len < 0 ? s.length : pos + len);

		if (global){
			m = Table.pack(r.exec(ss, pos + 1));
			var b = m[1] != null;
			if (b){
				this.s = s;
			}
			return b;
		} else {
			m = Table.pack(r.exec(ss, pos + 1));
			var b = m[1] != null;
			if (b){
				this.s = s;
			}
			return b;
		}
	}

	public function split( s : String ) : Array<String> {
		if (global){
			return Boot.luaIteratorToArray(Rex.split(s, r));
		} else {
			// we can't use directly Rex.split because it's ignoring the 'g' flag
			var d = "#__delim__#";
			return Boot.luaIteratorToArray(Rex.split(replace(s,d), d));
		}
	}

	public function replace( s : String, by : String ) : String {
		by = Rex.gsub(by, "\\$(\\d)", "%%1"); // convert dollar sign matched groups to Rex equivalent
		by = Rex.gsub(by, "\\${2}", "$"); // escape double dollar signs
		return Rex.gsub(s,r,by, global ? null : 1);
	}

	public function map( s : String, f : EReg -> String ) : String {
		var offset = 0;
		var buf = new StringBuf();
		do {
			if (offset >= s.length){
				break;
			}
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

	static function __init__() : Void {
		if (Rex == null){
			throw "Rex is missing.  Please install lrexlib-pcre.";
		}
	}

}

