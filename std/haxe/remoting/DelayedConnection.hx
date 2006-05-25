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
package haxe.remoting;

private signature InternalData {
	var ref : AsyncConnection;
	var msg : Array<{ path : Array<String>, params : Array<Dynamic>, onData : Dynamic -> Void }>;
}

class DelayedConnection extends AsyncConnection, implements Dynamic<DelayedConnection> {

	public var connection(getConnection,setConnection) : AsyncConnection;

	function __resolve( field : String ) : AsyncConnection {
		var d = new DelayedConnection(__data,__path.copy());
		d.__path.push(field);
		return d;
	}

	function getConnection() {
		var d : InternalData = __data;
		return d.ref;
	}

	function setConnection(cnx) {
		var d : InternalData = __data;
		d.ref = cnx;
		process(d);
		return cnx;
	}

	public function call( params, onData ) {
		var d : InternalData = __data;
		d.msg.push({ path : __path, params : params, onData : onData });
		process(d);
	}

	static function process( d : InternalData ) {
		if( d.ref == null )
			return;
		while( true ) {
			var m = d.msg.shift();
			if( m == null )
				break;
			var r = d.ref;
			for( p in m.path )
				r = r.__resolve(p);
			r.call(m.params,m.onData);
		}
	}

	public static function create() {
		var d : InternalData = { ref : null, msg : [] };
		return new DelayedConnection(d,[]);
	}

}
