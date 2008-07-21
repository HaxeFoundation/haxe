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