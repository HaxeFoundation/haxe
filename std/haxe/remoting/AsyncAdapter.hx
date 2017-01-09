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
package haxe.remoting;

/**
	Build an AsyncConnection from a synchronized Connection.
**/
class AsyncAdapter implements AsyncConnection {

	var __cnx : Connection;
	var __error : { ref : Dynamic -> Void };

	function new(cnx,error) {
		__cnx = cnx;
		__error = error;
	}

	public function resolve( name ) : AsyncConnection {
		return new AsyncAdapter(__cnx.resolve(name),__error);
	}

	public function setErrorHandler(h) {
		__error.ref = h;
	}

	public function call( params : Array<Dynamic>, ?onResult : Dynamic -> Void ) {
		var ret;
		try {
			ret = __cnx.call(params);
		} catch( e : Dynamic ) {
			__error.ref(e);
			return;
		}
		if( onResult != null ) onResult(ret);
	}

	public static function create( cnx : Connection ) : AsyncConnection {
		return new AsyncAdapter(cnx,{ ref : function(e) throw e });
	}

}
