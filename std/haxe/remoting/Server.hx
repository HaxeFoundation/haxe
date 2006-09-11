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

class Server {

	var objects : Hash<Dynamic>;
	var prefix : String;
	var log : String -> Void;

	public function new() {
		objects = new Hash();
		log = null;
	}

	public function addObject( name : String, obj : Dynamic ) {
		objects.set(name,obj);
	}

	public function setLogger( l ) {
		log = l;
	}

	public function setPrivatePrefix( p : String ) {
		prefix = p;
	}

	public function resolvePath( path : Array<String> ) : Dynamic {
		var objname = path.shift();
		if( objname == null )
			throw "Empty path";
		var obj = objects.get(objname);
		if( obj == null )
			throw "Object '"+objname+"' is not accessible";
		for( x in path ) {
			if( obj == null || (prefix != null && x.indexOf(prefix,0) == 0) )
				return null;
			obj = Reflect.field(obj,x);
		}
		return obj;
	}

	public function handleRequest() {
		if( neko.Web.getClientHeader("X-Haxe-Remoting") == null )
			return false;
		var v = neko.Web.getParams().get("__x");
		try {
			if( v == null )
				throw "Missing remoting data";
			var u = new haxe.Unserializer(v);
			var path : Array<String> = u.unserialize();
			var args : Array<Dynamic> = u.unserialize();
			var f = path.pop();
			var obj = resolvePath(path);
			var funptr = Reflect.field(obj,f);
			if( !Reflect.isFunction(funptr) )
				throw "Calling not-a-function '"+f+"'";
			var v = Reflect.callMethod(obj,funptr,args);
			var s = new haxe.Serializer();
			s.serialize(v);
			neko.Lib.print("hxr");
			neko.Lib.print(s.toString());
		} catch( e : Dynamic ) {
			if( log != null ) {
				log(haxe.Stack.toString(haxe.Stack.exceptionStack()));
				log(Std.string(e));
				log("\n\n");
			}
			var s = new haxe.Serializer();
			s.serializeException(e);
			neko.Lib.print("hxr");
			neko.Lib.print(s.toString());
		}
		return true;
	}

}

