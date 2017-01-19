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
import python.internal.UBuiltins;
import python.lib.Re;
import python.lib.Re.MatchObject;
import python.lib.Re.Pattern;

@:coreApi
class EReg {
	var pattern:Regex;
	var matchObj:MatchObject;
	var global:Bool;

	public function new( r : String, opt : String ) {
		global = false;
		var options = 0;
		for (i in 0...opt.length) {
			var c = StringTools.fastCodeAt(opt, i);
			if (c == "m".code) options |= Re.M;
			if (c == "i".code) options |= Re.I;
			if (c == "s".code) options |= Re.S;
			if (c == "u".code) options |= Re.U;
			if (c == "g".code) global = true;
		}
		pattern = Re.compile(r, options);
	}

	public inline function match( s : String ) : Bool {
		matchObj = Re.search(pattern, s);
		return matchObj != null;
	}

	public inline function matched( n : Int ) : String {
		return matchObj.group(n);
	}

	public inline function matchedLeft() : String {
		return matchObj.string.substr(0, matchObj.start());
	}

	public inline function matchedRight() : String {
		return matchObj.string.substr(matchObj.end());
	}

	public inline function matchedPos() : { pos : Int, len : Int } {
		return { pos : matchObj.start(), len : matchObj.end() - matchObj.start() };
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		if (len != -1) {
			matchObj = pattern.search(s, pos, pos+len);
		} else {
			matchObj = pattern.search(s, pos);
		}

		return this.matchObj != null;

	}

	public function split( s : String ) : Array<String> {
		return if (global) {
			var ret = [];
			var lastEnd = 0;

			for (x in Re.finditer(pattern, s)) {

				ret.push(s.substring(lastEnd, x.start() ));
				lastEnd = x.end();
			}
			ret.push(s.substr(lastEnd));
			ret;
		} else {
			this.match(s);
			if (matchObj == null) {
				[s];
			} else {
				[ s.substring(0, matchObj.start()), s.substr(matchObj.end()) ];
			}

		}
	}

	public function replace( s : String, by : String ) : String
	{
		var by = by.split("$$").join("_hx_#repl#__");
		function replace (x:MatchObject) {
			var res = by;
			var g = x.groups();
			for (i in 0...g.length) {
				var gs = g[i];
				if (gs == null)
					continue;
				res = res.split("$"+UBuiltins.str(i+1)).join(gs);
			}
			res = res.split("_hx_#repl#__").join("$");
			return res;
		}
		return Re.sub(pattern, replace, s, global ? 0 : 1 );
	}

	public function map( s : String, f : EReg -> String ) : String {

		var buf = new StringBuf();
		var pos = 0;
		var right = s;

		var cur = this;
		while( pos < s.length ) {

			if (matchObj == null) {
				matchObj = Re.search(pattern, s);
			} else {
				matchObj = matchObj.re.search(s, pos);
			}

			if( matchObj == null )
				break;



			var pos1 = matchObj.end();

			var curPos = cur.matchedPos();


			buf.add(cur.matchedLeft().substr(pos));
			buf.add(f(cur));

			right = cur.matchedRight();



			if (!global) {
				buf.add(right);
				return buf.toString();
			}

			if (curPos.len == 0) {
				buf.add(s.charAt(pos1));
				right = right.substr(1);
				pos = pos1+1;
			} else {
				pos = pos1;
			}


		}
		buf.add(right);
		return buf.toString();

	}
}
