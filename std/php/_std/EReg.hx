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
@:coreApi @:final class EReg {

	var r : Dynamic;
	var last : String;
	var global : Bool;
	var pattern : String;
	var options : String;
	var re : String;
	var matches : ArrayAccess<Dynamic>;

	public function new( r : String, opt : String ) : Void {
		this.pattern = r;
		var a = opt.split("g");
		global = a.length > 1;
		if( global )
			opt = a.join("");
		this.options = opt;
		this.re = untyped __php__("'\"' . str_replace('\"','\\\\\"',$r) . '\"' . $opt");
	}

	public function match( s : String ) : Bool {
		var p : Int = untyped __call__("preg_match", re, s, matches, __php__("PREG_OFFSET_CAPTURE"));

		if(p > 0)
			last = s;
		else
			last = null;
		return p > 0;
	}

	public function matched( n : Int ) : String {
		if (matches == null ||  n < 0 ) throw "EReg::matched";
		// we can't differenciate between optional groups at the end of a match
		// that have not been matched and invalid groups
		if( n >= untyped __call__("count", matches)) return null;
		if(untyped __php__("$this->matches[$n][1] < 0")) return null;
		return untyped __php__("$this->matches[$n][0]");
	}

	public function matchedLeft() : String {
		if( untyped __call__("count", matches) == 0 ) throw "No string matched";
		return last.substr(0, untyped __php__("$this->matches[0][1]"));
	}

	public function matchedRight() : String {
		if( untyped __call__("count", matches) == 0 ) throw "No string matched";
		var x : Int = untyped __php__("$this->matches[0][1]") + __call__("strlen",__php__("$this->matches[0][0]"));
		return last.substr(x);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		return untyped { pos : __php__("$this->matches[0][1]"), len : __call__("strlen",__php__("$this->matches[0][0]")) };
	}

	public function matchSub( s : String, pos : Int, len : Int = -1):Bool {
		var p : Int = untyped __call__("preg_match", re, len < 0 ? s : s.substr(0,pos + len), matches, __php__("PREG_OFFSET_CAPTURE"), pos);
		if(p > 0) {
			last = s;
		}
		else
			last = null;
		return p > 0;
	}

	public function split( s : String ) : Array<String> {
		return untyped __php__("new _hx_array(preg_split($this->re, $s, $this->{\"global\"} ? -1 : 2))");
	}

	public function replace( s : String, by : String ) : String {
		by = untyped __call__("str_replace", "\\$", "\\\\$", by);
		by = untyped __call__("str_replace", "$$", "\\$", by);
		untyped __php__("if(!preg_match('/\\\\([^?].*?\\\\)/', $this->re)) $by = preg_replace('/\\$(\\d+)/', '\\\\\\$\\1', $by)");
		return untyped __call__("preg_replace", re, by, s, global ? -1 : 1);
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
