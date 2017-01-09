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

class ContextAll extends Context {

	public override function call( path : Array<String>, params : Array<Dynamic> ) : Dynamic {
		#if neko
		var o : Dynamic = null;
		var m : Dynamic = neko.Lib.getClasses();
		for( p in path ) {
			o = m;
			m = Reflect.field(o,p);
		}
		#elseif js
		var path2 = path.copy();
		var f = path2.pop();
		var o;
		try {
			o = js.Lib.eval(path2.join("."));
		} catch( e : Dynamic ) {
			o = null;
		}
		var m = Reflect.field(o,f);
		#elseif flash
		var path2 = path.copy();
		var f = path2.pop();
		var o = flash.Lib.eval(path2.join("."));
		var m = Reflect.field(o,f);
		#elseif php
		var path2 = path.copy();
		var f = path2.pop();
		var o = Type.resolveClass(path2.join("."));
		var m = Reflect.field(o,f);
		#else
		var o = null;
		var m = null;
		#end
		if( m == null )
			return super.call(path,params);
		return Reflect.callMethod(o,m,params);
	}

}