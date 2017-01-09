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
    Allows communication between platforms. This is a shared API that can be called on the connection at the client code.
*/
class Context {

	var objects : haxe.ds.StringMap<{ obj : Dynamic, rec : Bool }>;

	public function new() {
		objects = new haxe.ds.StringMap();
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
