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

private typedef ERegValue = hl.Abstract<"ereg">;

@:access(String)
@:coreApi @:final class EReg {

	var r : ERegValue;
	var last : String;
	var global : Bool;

	public function new( r : String, opt : String ) : Void {
		var a = opt.split("g");
		global = a.length > 1;
		if( global )
			opt = a.join("");
		this.r = regexp_new_options(r.bytes, opt.bytes);
	}

	public function match( s : String ) : Bool {
		var p = regexp_match(r,s.bytes,0,s.length);
		if( p )
			last = s;
		else
			last = null;
		return p;
	}

	public function matched( n : Int ) : String {
		var len = 0;
		var m = regexp_matched_pos(r,n,len);
		return m < 0 ? null : last.substr(m, len);
	}

	public function matchedLeft() : String {
		var p = regexp_matched_pos(r,0,null);
		return last.substr(0,p);
	}

	public function matchedRight() : String {
		var len = 0;
		var p = regexp_matched_pos(r,0,len);
		return last.substr(p + len);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		var len = 0;
		var p = regexp_matched_pos(r, 0, len);
		if( p < 0 ) return null;
		return { pos : p, len : len };
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		var p = regexp_match(r, s.bytes, pos, len < 0 ? s.length - pos : len);
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
			if( !regexp_match(r,s.bytes,pos,len) )
				break;
			var plen = 0;
			var p = regexp_matched_pos(r,0,plen);
			if( plen == 0 && !first ) {
				if( p == s.length )
					break;
				p++;
			}
			a.push(s.substr(pos,p - pos));
			var tot = p + plen - pos;
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
			if( !regexp_match(r,s.bytes,pos,len) )
				break;
			var plen = 0;
			var p = regexp_matched_pos(r,0, plen);
			if( plen == 0 && !first ) {
				if( p == s.length )
					break;
				p++;
			}
			b.addSub(s,pos,p-pos);
			if( a.length > 0 )
				b.add(a[0]);
			var i = 1;
			while( i < a.length ) {
				var k = a[i];
				var c = StringTools.fastCodeAt(k, 0);
				// 1...9
				if( c >= 49 && c <= 57 ) {
					var plen = 0;
					var p = try regexp_matched_pos(r,Std.int(c)-48,plen) catch( e : String ) -1;
					if( p < 0 ){
						b.add("$");
						b.add(k);
					} else {
						if( p >= 0 ) b.addSub(s,p,plen);
						b.addSub(k,1,k.length - 1);
					}
				} else if( c == 0 ) {
					b.add("$");
					i++;
					var k2 = a[i];
					if( k2 != null && k2.length > 0 )
						b.add(k2);
				} else
					b.add("$"+k);
				i++;
			}
			var tot = p + plen - pos;
			pos += tot;
			len -= tot;
			first = false;
		} while( global );
		b.addSub(s,pos,len);
		return b.toString();
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
			var plen = 0;
			var p = regexp_matched_pos(r,0,plen);
			buf.add(s.substr(offset, p - offset));
			buf.add(f(this));
			if (plen == 0) {
				buf.add(s.substr(p, 1));
				offset = p + 1;
			}
			else
				offset = p + plen;
		} while (global);
		if (!global && offset > 0 && offset < s.length)
			buf.add(s.substr(offset));
		return buf.toString();
	}


	@:hlNative("std", "regexp_new_options") static function regexp_new_options( bytes : hl.Bytes, options : hl.Bytes ) : ERegValue {
		return null;
	}

	@:hlNative("std", "regexp_match") static function regexp_match( r : ERegValue, str : hl.Bytes, pos : Int, size : Int ) : Bool {
		return false;
	}

	@:hlNative("std", "regexp_matched_pos") static function regexp_matched_pos( r : ERegValue, n : Int, size : hl.Ref<Int> ) : Int {
		return 0;
	}


}
