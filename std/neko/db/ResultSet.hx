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
package neko.db;

class ResultSet implements Iterator<Dynamic> {

	public var length : Int;
	public var nfields : Int;
	private var __r : Void;
	private var cache : Dynamic;

	private function new(r) {
		__r = r;
		length = result_get_length(r);
		nfields = try result_get_nfields(r) catch( e : Dynamic ) 0;
	}

	public function hasNext() {
		if( cache == null )
			cache = next();
		return (cache != null);
	}

	public function next() : Dynamic {
		var c = cache;
		if( c != null ) {
			cache = null;
			return c;
		}
		c = result_next(__r);
		if( c == null )
			return null;
		untyped {
			var f = __dollar__objfields(c);
			var i = 0;
			var l = __dollar__asize(f);
			while( i < l ) {
				var v = __dollar__objget(c,f[i]);
				if( __dollar__typeof(v) == __dollar__tstring )
					__dollar__objset(c,f[i],new String(v));
				i = i + 1;
			}
		}
		return c;
	}

	public function getResult( n : Int ) {
		return new String(result_get(__r,n));
	}

	public function getIntResult( n : Int ) : Int {
		return result_get_int(__r,n);
	}

	public function getFloatResult( n : Int ) : Float {
		return result_get_float(__r,n);
	}

	private static var result_get_length = neko.Lib.load("mysql","result_get_length",1);
	private static var result_get_nfields = neko.Lib.load("mysql","result_get_nfields",1);
	private static var result_next = neko.Lib.load("mysql","result_next",1);
	private static var result_get = neko.Lib.load("mysql","result_get",2);
	private static var result_get_int = neko.Lib.load("mysql","result_get_int",2);
	private static var result_get_float = neko.Lib.load("mysql","result_get_float",2);
	private static var result_set_conv_date = neko.Lib.load("mysql","result_set_conv_date",2);

}
