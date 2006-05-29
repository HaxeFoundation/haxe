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

#if flash
#else error
#end

class LocalConnection extends AsyncConnection {

	var __funs : List<Dynamic -> Void>;

	function __resolve(field) : AsyncConnection {
		var s = new LocalConnection(__data,__path.copy());
		s.__error = __error;
		s.__funs = __funs;
		s.__path.push(field);
		return s;
	}

	public function call( params : Array<Dynamic>, onData : Dynamic -> Void ) : Void {
		try {
			var s = new haxe.Serializer();
			var p = __path.copy();
			var f = p.pop();
			if( f == null )
				throw "No method specified";
			s.serialize(params);
			if( !__data[untyped "send"](__data[untyped "target"],"remotingCall",p.join("."),f,s.toString()) )
				throw "Remoting call failure";
			__funs.add(onData);
		} catch( e : Dynamic ) {
			__error.ref(e);
		}
	}

	static function remotingCall( c : LocalConnection, path, f, args ) {
		var r = untyped Connection.doCall(path,f,args);
		if( !c.__data[untyped "send"](c.__data[untyped "target"],"remotingResult",r) )
			c.__error.ref("Remoting response failure");
	}

	static function remotingResult( c : LocalConnection, r : String ) {
		var f : Dynamic -> Void;
		var val : Dynamic;
		try {
			if( c.__funs.isEmpty() )
				throw "No response expected";
			f = c.__funs.pop();
			val = new haxe.Unserializer(r).unserialize();
			if( f == null )
				return;
		} catch( e : Dynamic ) {
			c.__error.ref(e);
			return;
		}
		f(val);
	}

	public static function connect( name : String ) {
		var l = new flash.LocalConnection();
		var c = new LocalConnection(l,[]);
		var recv = name+"_recv";
		Reflect.setField(l,"remotingCall",function(path,f,args) { remotingCall(c,path,f,args); });
		Reflect.setField(l,"remotingResult",function(r) { remotingResult(c,r); });
		l.onStatus = function(s:Dynamic) { if( s[untyped "level"] != "status" ) c.__error.ref("Failed to send data on LocalConnection"); }
		if( l.connect(name) )
			untyped l.target = recv;
		else {
			if( !l.connect(recv) )
				throw "Could not assign a LocalConnection to the name "+name;
			untyped l.target = name;
		}
		return c;
	}

}
