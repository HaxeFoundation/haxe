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

class LocalConnection implements AsyncConnection {

	static var ID = 0;

	var __path : Array<String>;
	var __data : {
		ctx : Context,
		results : IntHash<{ error : Dynamic -> Void, result : Dynamic -> Void }>,
		error : Dynamic -> Void,
		target : String,
		#if flash9
		cnx : flash.net.LocalConnection,
		#elseif flash
		cnx : flash.LocalConnection,
		#else
		cnx : Dynamic,
		#end
	};

	function new(data,path) {
		this.__path = path;
		this.__data = data;
	}

	public function resolve( name ) : AsyncConnection {
		var s = new LocalConnection(__data,__path.copy());
		s.__path.push(name);
		return s;
	}

	public function setErrorHandler(h) {
		__data.error = h;
	}

	public function call( params : Array<Dynamic>, ?onResult : Dynamic -> Void ) : Void {
		try {
			var id = ID++;
			#if flash9
			__data.cnx.send(__data.target,"remotingCall",id,__path.join("."),haxe.Serializer.run(params));
			#else
			if( !__data.cnx.send(__data.target,"remotingCall",id,__path.join("."),haxe.Serializer.run(params)) )
				throw "Remoting call failure";
			#end
			__data.results.set(id,{ error : __data.error, result : onResult });
		} catch( e : Dynamic ) {
			__data.error(e);
		}
	}

	public function close() {
		__data.cnx.close();
	}

	static function remotingCall( c : LocalConnection, id : Int, path : String, args : String ) {
		var r;
		try {
			var ret = c.__data.ctx.call(path.split("."),haxe.Unserializer.run(args));
			r = haxe.Serializer.run(ret);
		} catch( e : Dynamic ) {
			var s = new haxe.Serializer();
			s.serializeException(e);
			r = s.toString();
		}
		// don't forward 'send' errors on connection since it's only the receiving side
		c.__data.cnx.send(c.__data.target,"remotingResult",id,r);
	}

	static function remotingResult( c : LocalConnection, id : Int, result : String ) {
		var f = c.__data.results.get(id);
		if( f == null )
			c.__data.error("Invalid result ID "+id);
		c.__data.results.remove(id);
		var val : Dynamic;
		try {
			val = new haxe.Unserializer(result).unserialize();
		} catch( e : Dynamic ) {
			f.error(e);
			return;
		}
		if( f.result != null )
			f.result(val);
	}

	#if flash
	public static function connect( name : String, ctx : Context, ?allowDomains : Array<String> ) {
		#if flash9
			var l = new flash.net.LocalConnection();
		#else
			var l = new flash.LocalConnection();
		#end
		var recv = name + "_recv";
		var c = new LocalConnection({
			ctx : ctx,
			error : function(e) throw e,
			results : new IntHash(),
			cnx : l,
			target : recv,
		},[]);
		#if flash9
			l.client = {
				remotingCall : callback(remotingCall,c),
				remotingResult : callback(remotingResult,c),
			};
			l.addEventListener(flash.events.StatusEvent.STATUS, function(s:flash.events.StatusEvent) {
				if( s.level != "status" )
					c.__data.error("Failed to send data on LocalConnection");
			});
			try
				l.connect(name)
			catch( e : Dynamic ) {
				l.connect(recv);
				c.__data.target = name;
			}
			if( allowDomains != null )
				for( d in allowDomains )
					l.allowDomain(d);
		#else
			Reflect.setField(l,"remotingCall",callback(remotingCall,c));
			Reflect.setField(l,"remotingResult",callback(remotingResult,c));
			l.onStatus = function(s:Dynamic) {
				if( s[untyped "level"] != "status" )
					c.__data.error("Failed to send data on LocalConnection");
			};
			if( !l.connect(name) ) {
				if( !l.connect(recv) )
					throw "Could not assign a LocalConnection to the name "+name;
				c.__data.target = name;
			}
			if( allowDomains != null )
				l.allowDomain = function(dom) {
					for( d in allowDomains )
						if( d == dom )
							return true;
					return false;
				};
		#end
		return c;
	}
	#end

}
