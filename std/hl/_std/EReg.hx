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

private typedef ERegValue = hl.types.NativeAbstract<"ereg">;

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
		var p = regexp_match(r,s.bytes,0,s.size);
		if( p )
			last = s;
		else
			last = null;
		return p;
	}

	public function matched( n : Int ) : String {
		var size = 0;
		var m = regexp_matched(r,n,new hl.types.Ref(size));
		return m == null ? null : String.__alloc__(m,size,m.utf8Length(0,size));
	}

	public function matchedLeft() : String {
		var size = 0;
		var pos = regexp_matched_pos(r, 0, new hl.types.Ref(size));
		if( pos < 0 ) return null;
		return last.subBytes(0,pos);
	}

	public function matchedRight() : String {
		var size = 0;
		var pos = regexp_matched_pos(r, 0, new hl.types.Ref(size));
		if( pos < 0 ) return null;
		return last.subBytes(pos + size, last.size - (pos + size));
	}

	public function matchedPos() : { pos : Int, len : Int } {
		var len = 0;
		var pos = regexp_matched_pos(r, 0, new hl.types.Ref(len));
		if( pos < 0 ) return null;
		return { pos : last.bytes.utf8Length(0,pos), len : last.bytes.utf8Length(pos,len) };
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		if( pos < 0 ) pos = 0;
		if( pos > s.length ) pos = s.length;
		if( len < 0 || pos + len > s.length ) len = s.length - pos;
		var bpos = pos == 0 ? 0 : s.bytes.utf8Pos(0, pos);
		var blen = pos + len == s.length ? s.size - bpos : s.bytes.utf8Pos(bpos, len);
		var p = regexp_match(r, s.bytes, bpos, blen);
		if( p )
			last = s;
		else
			last = null;
		return p;
	}

	public function split( s : String ) : Array<String> {
		var pos = 0;
		var a = new Array();
		var first = true;
		var sbytes = s.bytes;
		var ssize = s.size;
		do {
			if( !regexp_match(r,sbytes,pos,ssize) )
				break;
			var msize = 0;
			var mpos = regexp_matched_pos(r, 0, new hl.types.Ref(msize));
			if( msize == 0 && !first ) {
				if( mpos == s.size )
					break;
				mpos++;
			}
			a.push(s.subBytes(pos,mpos - pos));
			var tot = mpos + msize - pos;
			pos += tot;
			ssize -= tot;
			first = false;
		} while( global );
		a.push(s.subBytes(pos,ssize));
		return a;
	}

	public function replace( s : String, by : String ) : String @:privateAccess {
		var b = new StringBuf();
		var pos = 0;
		var sbytes = s.bytes;
		var size = s.size;
		var a = by.split("$");
		var first = true;
		do {
			if( !regexp_match(r,sbytes,pos,size) )
				break;
			var msize = 0;
			var mpos = regexp_matched_pos(r,0,new hl.types.Ref(msize));
			if( msize == 0 && !first ) {
				if( mpos == s.size )
					break;
				mpos++;
			}
			b.__add(sbytes,pos,mpos-pos);
			if( a.length > 0 )
				b.add(a[0]);
			var i = 1;
			while( i < a.length ) {
				var k = a[i];
				var c = StringTools.fastCodeAt(k, 0);
				// 1...9
				if( c >= 49 && c <= 57 ) {
					var psize = 0;
					var p = try regexp_matched_pos(r,c-48, new hl.types.Ref(psize)) catch( e : String ) -1;
					if( p < 0 ){
						b.addChar("$".code);
						b.add(k);
					} else {
						if( p > 0 ) b.__add(sbytes, p, psize);
						b.addSub(k,1,k.length - 1);
					}
				} else if( c == 0 ) {
					b.addChar("$".code);
					i++;
					var k2 = a[i];
					if( k2 != null && k2.length > 0 )
						b.add(k2);
				} else
					b.add("$"+k);
				i++;
			}
			var tot = mpos + msize - pos;
			pos += tot;
			size -= tot;
			first = false;
		} while( global );
		b.__add(sbytes,pos,size);
		return b.toString();
	}

	public function map( s : String, f : EReg -> String ) : String @:privateAccess {
		var boffset = 0;
		var ssize = s.size;
		var buf = new StringBuf();
		do {
			if( boffset >= ssize )
				break;
			else if (!matchSub(s, s.bytes.utf8Length(0,boffset))) {
				buf.__add(s.bytes, boffset, ssize - boffset);
				break;
			}
			var msize = 0;
			var mpos = regexp_matched_pos(r,0, new hl.types.Ref(msize));
			buf.__add(s.bytes, boffset, mpos - boffset);
			buf.add(f(this));
			if( msize == 0 ) {
				if( mpos == ssize ) break;
				var k = s.bytes.utf8Pos(mpos, 1);
				buf.__add(s.bytes, mpos, k);
				boffset = mpos + k;
			} else
				boffset = mpos + msize;
		} while (global);
		if (!global && boffset > 0 && boffset < ssize )
			buf.__add(s.bytes, boffset, ssize - boffset);
		return buf.toString();
	}

	@:hlNative("regexp", "regexp_new_options") static function regexp_new_options( bytes : hl.types.Bytes, options : hl.types.Bytes ) : ERegValue {
		return null;
	}

	@:hlNative("regexp", "regexp_match") static function regexp_match( r : ERegValue, str : hl.types.Bytes, pos : Int, size : Int ) : Bool {
		return false;
	}

	@:hlNative("regexp", "regexp_matched") static function regexp_matched( r : ERegValue, n : Int, size : hl.types.Ref<Int> ) : hl.types.Bytes {
		return null;
	}

	@:hlNative("regexp", "regexp_matched_pos") static function regexp_matched_pos( r : ERegValue, n : Int, size : hl.types.Ref<Int> ) : Int {
		return 0;
	}
}
