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
