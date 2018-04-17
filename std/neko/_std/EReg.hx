/*
 * Copyright (C)2005-2018 Haxe Foundation
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
@:coreApi @:final class EReg {

	var r : Dynamic;
	var last : String;
	var global : Bool;

	public function new( r : String, opt : String ) : Void {
		var a = opt.split("g");
		global = a.length > 1;
		if( global )
			opt = a.join("");
		this.r = regexp_new_options(untyped r.__s, untyped opt.__s);
	}

	public function match( s : String ) : Bool {
		var p = regexp_match(r,untyped s.__s,0,s.length);
		if( p )
			last = s;
		else
			last = null;
		return p;
	}

	public function matched( n : Int ) : String {
		var m = regexp_matched(r,n);
		return (m == null) ? null : new String(m);
	}

	public function matchedLeft() : String {
		var p = regexp_matched_pos(r,0);
		return last.substr(0,p.pos);
	}

	public function matchedRight() : String {
		var p = regexp_matched_pos(r,0);
		var sz = p.pos+p.len;
		return last.substr(sz,last.length-sz);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		return regexp_matched_pos(r,0);
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		var p = regexp_match(r, untyped s.__s, pos, len < 0 ? s.length - pos : len);
		if( p )
			last = s;
		else
			last = null;
		return p;
	}

	public function split( s : String ) : Array<String> {
		var pos = 0;
		var len = s.length;
		var a = new Array();
		var first = true;
		do {
			if( !regexp_match(r,untyped s.__s,pos,len) )
				break;
			var p = regexp_matched_pos(r,0);
			if( p.len == 0 && !first ) {
				if( p.pos == s.length )
					break;
				p.pos += 1;
			}
			a.push(s.substr(pos,p.pos - pos));
			var tot = p.pos + p.len - pos;
			pos += tot;
			len -= tot;
			first = false;
		} while( global );
		a.push(s.substr(pos,len));
		return a;
	}

	public function replace( s : String, by : String ) : String {
		var b = new StringBuf();
		var pos = 0;
		var len = s.length;
		var a = by.split("$");
		var first = true;
		do {
			if( !regexp_match(r,untyped s.__s,pos,len) )
				break;
			var p = regexp_matched_pos(r,0);
			if( p.len == 0 && !first ) {
				if( p.pos == s.length )
					break;
				p.pos += 1;
			}
			b.addSub(s,pos,p.pos-pos);
			if( a.length > 0 )
				b.add(a[0]);
			var i = 1;
			while( i < a.length ) {
				var k = a[i];
				var c = k.charCodeAt(0);
				// 1...9
				if( c >= 49 && c <= 57 ) {
					var p = try regexp_matched_pos(r,Std.int(c)-48) catch( e : String ) null;
					if( p == null ){
						b.add("$");
						b.add(k);
					} else {
						if( p.pos >= 0 ) b.addSub(s,p.pos,p.len);
						b.addSub(k,1,k.length - 1);
					}
				} else if( c == null ) {
					b.add("$");
					i++;
					var k2 = a[i];
					if( k2 != null && k2.length > 0 )
						b.add(k2);
				} else
					b.add("$"+k);
				i++;
			}
			var tot = p.pos + p.len - pos;
			pos += tot;
			len -= tot;
			first = false;
		} while( global );
		b.addSub(s,pos,len);
		return b.toString();
	}

	//public function map( s : String, f : EReg -> String ) : String {
		//var b = new StringBuf();
		//var pos = 0;
		//var len = s.length;
		//var first = true;
		//last = s;
		//do {
			//if( !regexp_match(r,untyped s.__s,pos,len) )
				//break;
			//var p = regexp_matched_pos(r,0);
			//if( p.len == 0 && !first ) {
				//if( p.pos == s.length )
					//break;
				//p.pos += 1;
			//}
			//b.addSub(s,pos,p.pos-pos);
			//b.add(f(this));
			//var tot = p.pos + p.len - pos;
			//pos += tot;
			//len -= tot;
			//first = false;
		//} while( global );
		//b.addSub(s,pos,len);
		//return b.toString();
	//}

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
			var p = regexp_matched_pos(r,0);
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

	public static function escape( s : String ) : String {
		return escapeRegExpRe.map(s, function(r) return "\\" + r.matched(0));
	}
	static var escapeRegExpRe = ~/[\[\]{}()*+?.\\\^$|]/g;

	static var regexp_new_options = neko.Lib.load("regexp","regexp_new_options",2);
	static var regexp_match = neko.Lib.load("regexp","regexp_match",4);
	static var regexp_matched = neko.Lib.load("regexp","regexp_matched",2);
	static var regexp_matched_pos : Dynamic -> Int -> { pos : Int, len : Int } = neko.Lib.load("regexp","regexp_matched_pos",2);
}
