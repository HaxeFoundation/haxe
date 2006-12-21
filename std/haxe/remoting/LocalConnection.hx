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

	override function __resolve(field) : AsyncConnection {
		var s = new LocalConnection(__data,__path.copy());
		s.__error = __error;
		s.__funs = __funs;
		s.__path.push(field);
		return s;
	}

	override public function call( params : Array<Dynamic>, ?onData : Dynamic -> Void ) : Void {
		try {
			var s = new haxe.Serializer();
			var p = __path.copy();
			var f = p.pop();
			if( f == null )
				throw "No method specified";
			s.serialize(params);
			#if flash9
			__data.send(__data.client.target,"remotingCall",p.join("."),f,s.toString());
			#else true
			if( !__data[untyped "send"](__data.target,"remotingCall",p.join("."),f,s.toString()) )
				throw "Remoting call failure";
			#end
			__funs.add(onData);
		} catch( e : Dynamic ) {
			__error.ref(e);
		}
	}

	public function closeConnection() {
		#if flash9
		var cnx : flash.net.LocalConnection = __data;
		#else true
		var cnx : flash.LocalConnection = __data;
		#end
		cnx.close();
	}

	static function remotingCall( c : LocalConnection, path, f, args ) {
		var r = untyped Connection.doCall(path,f,args);
		#if flash9
		c.__data.send(c.__data.client.target,"remotingResult",r);
		#else true
		if( !c.__data[untyped "send"](c.__data.target,"remotingResult",r) )
			c.__error.ref("Remoting response failure");
		#end
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

	public static function connect( name : String, ?allowDomains : Array<String> ) {
		#if flash9
		var l = new flash.net.LocalConnection();
		#else flash
		var l = new flash.LocalConnection();
		#end
		var c = new LocalConnection(l,[]);
		c.__funs = new List();
		var recv = name+"_recv";
		var api = {
			remotingCall : function(path,f,args) { remotingCall(c,path,f,args); },
			remotingResult : function(r) { remotingResult(c,r); },
			onStatus : function(s:Dynamic) { if( s[untyped "level"] != "status" ) c.__error.ref("Failed to send data on LocalConnection"); },
			target : null,
		}
		#if flash9
		l.client = api;
		l.addEventListener(flash.events.StatusEvent.STATUS, api.onStatus);
		try {
			api.target = recv;
			l.connect(name);
		} catch( e : Dynamic ) {
			api.target = name;
			l.connect(recv);
		}
		#else true
		Reflect.setField(l,"remotingCall",api.remotingCall);
		Reflect.setField(l,"remotingResult",api.remotingResult);
		l.onStatus = api.onStatus;
		if( allowDomains != null )
		#if flash9
			for( d in allowDomains )
				l.allowDomain(d);
		#else true
			l.allowDomain = function(dom) {
				for( d in allowDomains )
					if( d == dom )
						return true;
				return false;
			};
		#end
		if( l.connect(name) )
			untyped l.target = recv;
		else {
			if( !l.connect(recv) )
				throw "Could not assign a LocalConnection to the name "+name;
			untyped l.target = name;
		}
		#end
		return c;
	}

}
