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

	public function customReplace( s : String, f : EReg -> String ) : String {
		var b = new StringBuf();
		var pos = 0;
		var len = s.length;
		var first = true;
		last = s;
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
			b.add(f(this));
			var tot = p.pos + p.len - pos;
			pos += tot;
			len -= tot;
			first = false;
		} while( true );
		b.addSub(s,pos,len);
		return b.toString();
	}

	static var regexp_new_options = neko.Lib.load("regexp","regexp_new_options",2);
	static var regexp_match = neko.Lib.load("regexp","regexp_match",4);
	static var regexp_matched = neko.Lib.load("regexp","regexp_matched",2);
	static var regexp_matched_pos : Dynamic -> Int -> { pos : Int, len : Int } = neko.Lib.load("regexp","regexp_matched_pos",2);

}
