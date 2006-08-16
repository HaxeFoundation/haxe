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
package neko;

class Boot {

	private static function __tmp_str() {
		return untyped "<...>".__s;
	}

	private static function __enum_str(e : Dynamic) {
		if( e.args == null )
			return e.tag;
		var s = e.tag + untyped "(".__s;
		var i = 0;
		var l = untyped __dollar__asize(e.args);
		var old = e.__string;
		e.__string = __tmp_str;
		while( i < l ) {
			if( i != 0 )
				s += untyped ",".__s;
			try {
				s += untyped __dollar__string(e.args[i]);
			} catch( e : Dynamic ) {
				e.__string = old;
				throw e;
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

	private static function __instanceof(o,cl) {
		untyped {
			if( cl == Dynamic )
				return true;
			switch __dollar__typeof(o) {
			case __dollar__tint: return (cl == Int || cl == Float);
			case __dollar__tfloat: return cl == Float;
			case __dollar__tbool: return cl == Bool;
			case __dollar__tobject:
				return __interfLoop(o.__class__,cl) || ( o.__enum__ == cl );
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
					throw ("Class not found " + neko.Lib.string(v));
			}
			if( cl.__name__ != null || cl.__ename__ != null )
				return cl.prototype;
			throw "Invalid class " + Std.string(v);
		}
	}

	private static function __init() {
		untyped {
			String = NekoString__;
			Array = NekoArray__;
			Int = __dollar__new(null);
			Float = __dollar__new(null);
			Bool = __dollar__new(null);
			Dynamic = __dollar__new(null);
			__dollar__objset(Bool,__dollar__hash("true".__s),true);
			__dollar__objset(Bool,__dollar__hash("false".__s),false);
			__dollar__exports.__unserialize = __unserialize;
		}
	}

}
