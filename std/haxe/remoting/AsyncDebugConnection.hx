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

class AsyncDebugConnection implements AsyncConnection implements Dynamic<AsyncDebugConnection> {

	var __path : Array<String>;
	var __cnx : AsyncConnection;
	var __data : {
		error : Dynamic -> Void,
		oncall : Array<String> -> Array<Dynamic> -> Void,
		onerror : Array<String> -> Array<Dynamic> -> Dynamic -> Void,
		onresult : Array<String> -> Array<Dynamic> -> Dynamic -> Void,
	};

	function new(path,cnx,data) {
		__path = path;
		__cnx = cnx;
		__data = data;
	}

	public function resolve( name ) : AsyncConnection {
		var cnx = new AsyncDebugConnection(__path.copy(),__cnx.resolve(name),__data);
		cnx.__path.push(name);
		return cnx;
	}

	public function setErrorHandler(h) {
		__data.error = h;
	}

	public function setErrorDebug(h) {
		__data.onerror = h;
	}

	public function setResultDebug(h) {
		__data.onresult = h;
	}

	public function setCallDebug(h) {
		__data.oncall = h;
	}

	public function call( params : Array<Dynamic>, ?onResult : Dynamic -> Void ) {
		var me = this;
		__data.oncall(__path,params);
		__cnx.setErrorHandler(function(e) {
			me.__data.onerror(me.__path,params,e);
			me.__data.error(e);
		});
		__cnx.call(params,function(r) {
			me.__data.onresult(me.__path,params,r);
			if( onResult != null ) onResult(r);
		});
	}

	public static function create( cnx : AsyncConnection ) {
		var cnx = new AsyncDebugConnection([],cnx,{
			error : function(e) throw e,
			oncall : function(path,params) {},
			onerror : null,
			onresult : null,
		});
		cnx.setErrorDebug(function(path,params,e) trace(path.join(".")+"("+params.join(",")+") = ERROR "+Std.string(e)));
		cnx.setResultDebug(function(path,params,e) trace(path.join(".")+"("+params.join(",")+") = "+Std.string(e)));
		return cnx;
	}

}
