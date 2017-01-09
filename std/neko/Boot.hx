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
package neko;

@:dox(hide)
@:keep
class Boot {

	private static function __tmp_str() {
		return untyped "<...>".__s;
	}

	private static function __enum_str(e : Dynamic) {
		if( e.args == null )
			return e.tag;
		var s : String = e.tag + untyped "(".__s;
		var i = 0;
		var l = untyped __dollar__asize(e.args);
		var old = e.__string;
		e.__string = __tmp_str;
		while( i < l ) {
			if( i != 0 )
				s += untyped ",".__s;
			try {
				s += untyped __dollar__string(e.args[i]);
			} catch( err : Dynamic ) {
				s += __tmp_str();
			}
			i += 1;
		}
		e.__string = old;
		return s + untyped ")".__s;
	}

	private static function __interfLoop(cc : Dynamic,cl : Dynamic) {
		if( cc == null )
			return false;
		if( cc == cl )
			return true;
		var intf : Dynamic = cc.__interfaces__;
		if( intf != null )
			for( i in 0...intf.length ) {
				var i = intf[i];
				if( i == cl || __interfLoop(i,cl) )
					return true;
			}
		return __interfLoop(cc.__super__,cl);
	}

	@:ifFeature("typed_catch")
	private static function __instanceof(o,cl) {
		untyped {
			if( cl == Dynamic )
				return true;
			switch( __dollar__typeof(o) ) {
			case 1: return (cl == Int || cl == Float);
			case 2: return cl == Float || (cl == Int && __dollar__int(o) == o);
			case 3: return cl == Bool;
			case 5:
				if( cl == null )
					return false;
				return __interfLoop(o.__class__,cl) || ( o.__enum__ == cl ) || (cl == Class && o.__name__ != null) || (cl == Enum && o.__ename__ != null );
			default:
				return false;
			}
		}
	}

	private static function __serialize(o) {
		untyped {
			if( o.__class__ != null ) {
				var n = o.__class__.__name__;
				var x = __dollar__amake(n.length);
				for( i in 0...n.length )
					x[i] = n[i].__s;
				return x;
			}
			if( o.__enum__ != null ) {
				var n = o.__enum__.__ename__;
				var x = __dollar__amake(n.length);
				for( i in 0...n.length )
					x[i] = n[i].__s;
				return x;
			}
			throw "Can't serialize";
		}
	}

	private static function __tagserialize(o) untyped {
		var n = o.__enum__.__ename__;
		var x = __dollar__amake(n.length + 1);
		for( i in 0...n.length )
			x[i] = n[i].__s;
		x[n.length] = o.tag;
		return x;
	}

	private static function __unserialize(v) {
		untyped {
			if( __dollar__typeof(v) != __dollar__tarray )
				throw "Invalid serialized class data";
			for( i in 0...__dollar__asize(v) )
				if( __dollar__typeof(v[i]) != __dollar__tstring )
					throw "Invalid serialized class data";
			var cl = neko.Boot.__classes;
			for( i in 0...__dollar__asize(v) ) {
				cl = __dollar__objget(cl,__dollar__hash(v[i]));
				if( cl == null )
					throw ("Class not found " + Std.string(v));
			}
			if( __dollar__typeof(cl) == __dollar__tobject ) {
				if( cl.__name__ != null || cl.__ename__ != null )
					return cl.prototype;
				if( cl.__enum__ != null && __dollar__typeof(cl.tag) == __dollar__tstring )
					return cl;
			}
			throw "Invalid class " + Std.string(v);
		}
	}

	private static function __init() {
		untyped {
			__dollar__exports.__unserialize = __unserialize;
			__dollar__exports.__classes = neko.Boot.__classes;
		}
	}

}
