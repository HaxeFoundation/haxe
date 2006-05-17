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
package haxe;

class AsyncDebugConnection implements AsyncConnection, implements Dynamic<AsyncDebugConnection> {

	var __data : Dynamic;
	var __path : Array<String>; // not used here

	public function new(cnx) {
		__data = cnx;
		onError = cnx.onError;
		var me = this;
		cnx.onError = function(e) {
			me.onError(e);
		}
	}

	function __resolve(field : String) : AsyncConnection {
		var s = new AsyncDebugConnection(__data.__resolve(field));
		s.onError = onError;
		return s;
	}

	public function onError( err : Dynamic ) {
	}

	public function onTrace( t : String ) {
		trace(t);
	}

	public function eval( onData : Dynamic -> Void ) : Void {
		onTrace(__data.__path.join("."));
		__data.eval(onData);
	}

	public function call( params : Array<Dynamic>, onData : Dynamic -> Void ) : Void {
		onTrace(__data.__path.join(".")+"("+params.join(",")+")");
		__data.call(params,onData);
	}

}
