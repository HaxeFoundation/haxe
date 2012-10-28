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
@:coreApi class EReg {

	var r : flash.utils.RegExp;
	var result : {> Array<String>, index : Int, input : String };

	public function new( r : String, opt : String ) : Void {
		this.r = new flash.utils.RegExp(r,opt);
	}

	public function match( s : String ) : Bool {
		if( r.global ) r.lastIndex = 0;
		result = r.exec(s);
		return (result != null);
	}

	public function matched( n : Int ) : String {
		return if( result != null && n >= 0 && n < result.length ) result[n] else throw "EReg::matched";
	}

	public function matchedLeft() : String {
		if( result == null ) throw "No string matched";
		var s = result.input;
		return s.substr(0,result.index);
	}

	public function matchedRight() : String {
		if( result == null ) throw "No string matched";
		var rl = result.index + result[0].length;
		var s = result.input;
		return s.substr(rl,s.length - rl);
	}

	public function matchedPos() : { pos : Int, len : Int } {
		if( result == null ) throw "No string matched";
		return { pos : result.index, len : result[0].length };
	}

	public function split( s : String ) : Array<String> {
		// we can't use directly s.split because it's ignoring the 'g' flag
		var d = "#__delim__#";
		var s : String = untyped s.replace(r, d);
		return s.split(d);
	}

	public function replace( s : String, by : String ) : String {
		return untyped s.replace(r,by);
	}

	public function customReplace( s : String, f : EReg -> String ) : String {
		var buf = new StringBuf();
		while( true ) {
			if( !match(s) )
				break;
			buf.add(matchedLeft());
			buf.add(f(this));
			s = matchedRight();
		}
		buf.add(s);
		return buf.toString();
	}

}
