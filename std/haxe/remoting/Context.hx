/*
 * Copyright (c) 2005-2008, The haXe Project Contributors
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

class Context {

	var objects : Hash<{ obj : Dynamic, rec : Bool }>;

	public function new() {
		objects = new Hash();
	}

	public function addObject( name : String, obj : {}, ?recursive ) {
		objects.set(name,{ obj : obj, rec : recursive });
	}

	public function call( path : Array<String>, params : Array<Dynamic> ) : Dynamic {
		if( path.length < 2 ) throw "Invalid path '"+path.join(".")+"'";
		var inf = objects.get(path[0]);
		if( inf == null )
			throw "No such object "+path[0];
		var o = inf.obj;
		var m = Reflect.field(o,path[1]);
		if( path.length > 2 ) {
			if( !inf.rec ) throw "Can't access "+path.join(".");
			for( i in 2...path.length ) {
				o = m;
				m = Reflect.field(o,path[i]);
			}
		}
		if( !Reflect.isFunction(m) )
			throw "No such method "+path.join(".");
		return Reflect.callMethod(o,m,params);
	}

	public static function share( name : String, obj : {} ) : Context {
		var ctx = new Context();
		ctx.addObject(name,obj);
		return ctx;
	}

}