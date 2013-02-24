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
import java.util.regex.Regex;
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

@:coreType class EReg {

	private var pattern:String;
	private var matcher:Matcher;
	private var cur:String;
	private var sub:Int;
	private var isGlobal:Bool;

	public function new( r : String, opt : String ) {
		var flags = 0;
		for (i in 0...opt.length)
		{
			switch(StringTools.fastCodeAt(opt, i))
			{
				case 'i'.code:
					flags |= Pattern.CASE_INSENSITIVE;
				case 'm'.code:
					flags |= Pattern.MULTILINE;
				case 's'.code:
					flags |= Pattern.DOTALL;
				case 'g'.code:
					isGlobal = true;
			}
		}

		matcher = Pattern.compile(convert(r), flags).matcher("");
		pattern = r;
	}

	private static function convert(r:String):String
	{
		//some references of the implementation:
		//http://stackoverflow.com/questions/809647/java-vs-javascript-regex-problem
		//http://stackoverflow.com/questions/4788413/how-to-convert-javascript-regex-to-safe-java-regex
		//Some necessary changes:
		//
		// \0  -> \x00
		// \v  -> \x0b
		// [^] -> [\s\S]
		// unescaped ', " -> \', \"
		/* FIXME
		var pat = new StringBuf();
		var len = r.length;
		var i = 0;
		while (i < len)
		{
			var c = StringTools.fastCodeAt(r, i++);
			switch(c)
			{
				case '\\'.code: //escape-sequence

			}
		}
		*/
		return r;
	}

	public function match( s : String ) : Bool {
		sub = 0;
		cur = s;
		matcher = matcher.reset(s);
		return matcher.find();
	}

	public function matched( n : Int ) : String
	{
		if (n == 0)
			return matcher.group();
		else
			return matcher.group(n);
	}

	public function matchedLeft() : String
	{
		return untyped cur.substring(0, sub + matcher.start());
	}

	public function matchedRight() : String
	{
		return untyped cur.substring(sub + matcher.end(), cur.length);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		var start = matcher.start();
		return { pos : sub + start, len : matcher.end() - start };
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		var s2 = (len < 0 ? s.substr(pos) : s.substr(pos, len));
		sub = pos;
		matcher = matcher.reset(s2);
		cur = s;
		return matcher.find();
	}

	public function split( s : String ) : Array<String>
	{
		if (isGlobal)
		{
			var ret = [];
			while(this.match(s))
			{
				ret.push(matchedLeft());
				s = matchedRight();
			}
			ret.push(s);
			return ret;
		} else {
			var m = matcher;
			m.reset(s);
			if (m.find())
			{
				return untyped [s.substring(0, m.start()), s.substring(m.end(), s.length)];
			} else {
				return [s];
			}
		}
	}

	inline function start(group:Int)
	{
		return matcher.start(group) + sub;
	}

	inline function len(group:Int)
	{
		return matcher.end(group) - matcher.start(group);
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
