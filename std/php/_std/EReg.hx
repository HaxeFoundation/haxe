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
		if ( n < 0 ) throw "EReg::matched";
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

	public function split( s : String ) : Array<String> {
		return untyped __php__("new _hx_array(preg_split($this->re, $s, $this->hglobal ? -1 : 2))");
	}

	public function replace( s : String, by : String ) : String {
		by = untyped __call__("str_replace", "\\$", "\\\\$", by);
		by = untyped __call__("str_replace", "$$", "\\$", by);
		untyped __php__("if(!preg_match('/\\\\([^?].+?\\\\)/', $this->re)) $by = preg_replace('/\\$(\\d+)/', '\\\\\\$\\1', $by)");
		return untyped __call__("preg_replace", re, by, s, global ? -1 : 1);
	}

	public function customReplace( s : String, f : EReg -> String ) : String {
		var buf = "";
		while( true ) {
			if( !match(s) )
				break;
			buf += matchedLeft();
			buf += f(this);
			s = matchedRight();
		}
		buf += s;
		return buf;
	}
}
