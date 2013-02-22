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
import cs.system.text.regularExpressions.Regex;

class EReg {

	private var regex : Regex;
	private var m : Match;
	private var isGlobal : Bool;
	private var cur : String;
	private var sub : Int;

	public function new( r : String, opt : String ) : Void {
		var opts:Int = cast CultureInvariant;
		for (i in 0...opt.length) untyped {
			switch(cast(opt[i], Int))
			{
				case 'i'.code:
					opts |= cast(IgnoreCase, Int);
				case 'g'.code:
					isGlobal = true;
				case 'm'.code:
					opts |= cast(Multiline, Int);
				case 'c'.code:
					opts |= cast(Compiled, Int);
			}
		}

		this.regex = new Regex(r, cast(opts, RegexOptions));
	}

	public function match( s : String ) : Bool {
		sub = 0;
		m = regex.Match(s);
		cur = s;
		return m.Success;
	}

	public function matched( n : Int ) : String {
		if (m == null || cast(n, UInt) > m.Groups.Count)
			throw "EReg::matched";
		return m.Groups[n].Value;
	}

	public function matchedLeft() : String {
		return untyped cur.Substring(0, sub + m.Index);
	}

	public function matchedRight() : String {
		return untyped cur.Substring(sub + m.Index + m.Length);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		return { pos : sub + m.Index, len : m.Length };
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		var s2 = (len < 0 ? s.substr(pos) : s.substr(pos, len));
		sub = pos;
		m = regex.Match(s2);
		cur = s;
		return m.Success;
	}

	public function split( s : String ) : Array<String> {
		if (isGlobal)
			return cs.Lib.array(regex.Split(s));
		var m = regex.Match(s);
		return untyped [s.Substring(0, m.Index), s.Substring(m.Index + m.Length)];
	}

	inline function start(group:Int)
	{
		return m.Groups[group].Index + sub;
	}

	inline function len(group:Int)
	{
		return m.Groups[group].Length;
	}

	public function replace( s : String, by : String ) : String
	{
      var b = new StringBuf();
      var pos = 0;
      var len = s.length;
      var a = by.split("$");
      var first = true;
      do {
        if( !matchSub(s,pos,len) )
          break;
        var p = matchedPos();
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
						try {
							var ppos = start( c-48 ), plen = this.len( c-48 );
							b.addSub(s, ppos, plen);
						}
						catch(e:Dynamic)
						{
							b.add("$");
							b.add(k);
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
      } while( isGlobal );
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
			var p = matchedPos();
			buf.add(s.substr(offset, p.pos - offset));
			buf.add(f(this));
			if (p.len == 0) {
				buf.add(s.substr(p.pos, 1));
				offset = p.pos + 1;
			}
			else
				offset = p.pos + p.len;
		} while (isGlobal);
		if (!isGlobal && offset < s.length)
			buf.add(s.substr(offset));
		return buf.toString();
	}
}
