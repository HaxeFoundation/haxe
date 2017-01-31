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

class DelayedConnection implements AsyncConnection implements Dynamic<AsyncConnection> {

	public var connection(get,set) : AsyncConnection;

	var __path : Array<String>;
	var __data : {
		cnx : AsyncConnection,
		error : Dynamic -> Void,
		cache : Array<{
			path : Array<String>,
			params : Array<Dynamic>,
			onResult : Dynamic -> Void,
			onError : Dynamic -> Void
		}>,
	};

	function new(data,path) {
		__data = data;
		__path = path;
	}

	public function setErrorHandler(h) {
		__data.error = h;
	}

	public function resolve( name ) : AsyncConnection {
		var d = new DelayedConnection(__data,__path.copy());
		d.__path.push(name);
		return d;
	}

	function get_connection() {
		return __data.cnx;
	}

	function set_connection(cnx) {
		__data.cnx = cnx;
		process(this);
		return cnx;
	}

	public function call( params : Array<Dynamic>, ?onResult ) {
		__data.cache.push({ path : __path, params : params, onResult : onResult, onError : __data.error });
		process(this);
	}

	static function process( d : DelayedConnection ) {
		var cnx = d.__data.cnx;
		if( cnx == null )
			return;
		while( true ) {
			var m = d.__data.cache.shift();
			if( m == null )
				break;
			var c = cnx;
			for( p in m.path )
				c = c.resolve(p);
			c.setErrorHandler(m.onError);
			c.call(m.params,m.onResult);
		}
	}

	public static function create() {
		return new DelayedConnection({ cnx : null, error : function(e) throw e, cache : new Array() },[]);
	}

}
