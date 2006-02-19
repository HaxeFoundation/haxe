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

class Std {

	public static var infinity = 1.0 / 0.0;
	public static var nan = 0.0 / 0.0;

	public static function instanceof( obj : Dynamic, vclass : Dynamic ) : Bool {
		return untyped
		#if flash
		flash.Boot.__instanceof(obj,vclass);
		#else neko
		neko.Boot.__instanceof(obj,vclass);
		#else error
		#end
	}

	public static function string( s : Dynamic ) : String {
		return untyped
		#if flash
		flash.Boot.__string_rec(s,"");
		#else neko
		__dollar__string(s);
		#else error
		#end
	}

	public static function int( x : Float ) : Int {
		return Math.floor(x);
	}

	public static function bool( x : Dynamic ) : Bool {
		return x != 0 && x != null && x != false;
	}

	public static function parseInt( x : String ) : Int {
		return untyped
		#if flash
		_global.parseInt(x);
		#else neko
		__dollar__int(x.__s);
		#else error
		#end
	}

	public static function parseFloat( x : String ) : Float {
		return untyped
		#if flash
		_global.parseFloat(x);
		#else neko
		__dollar__float(x.__s);
		#else error
		#end
	}

	public static function chr( x : Int ) : String {
		return untyped
		#if flash
		String.fromCharCode(x);
		#else neko
		{
			var s = __dollar__smake(1);
			__dollar__sset(s,0,x);
			new String(s);
		}
		#else error
		#end
	}

	public static function ord( x : String ) : Int {
		return untyped
		#if flash
		if( x == "" )
			null;
		else
			x.charCodeAt(0);
		#else neko
		{
			var s = __dollar__ssize(x.__s);
			if( s == 0 )
				null;
			else
				__dollar__sget(s,0);
		}
		#else error
		#end
	}

	public static function isFinite(i : Float) : Bool {
		return untyped
		#if flash
		_global.isFinite(i);
		#else neko
		!__dollar__isinfinite(i);
		#else error
		#end
	}

	public static function isNaN(i : Float) : Bool {
		return untyped
		#if flash
		_global.isNaN(i);
		#else neko
		__dollar__isnan(i);
		#else error
		#end
	}

	public static function random( x : Int ) : Int {
		return untyped
		#if flash
		__random__(x);
		#else neko
		Math._rand_int(Math._rnd,x);
		#else error
		#end
	}

}
