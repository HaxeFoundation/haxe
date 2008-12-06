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

/**
	A String buffer is an efficient way to build a big string by
	appending small elements together.
**/
class StringBuf {

	/**
		Creates a new string buffer.
	**/
	public function new() {
		#if neko
			b = __make();
		#elseif js
			b = new Array();
		#else
			b = "";
		#end
	}

	/**
		Adds the representation of any value to the string buffer.
	**/
	public inline function add( ?x : Dynamic ) {
		#if neko
			__add(b,x);
		#elseif js
			b[b.length] = x;
		#else
			b += x;
		#end
	}

	/**
		Adds a part of a string to the string buffer.
	**/
	public inline function addSub( s : String, pos : Int, ?len : Int ) {
		#if neko
			__add_sub(b,untyped s.__s,pos,len == null ? s.length - pos : len);
		#elseif flash9
			if( len == null )
				b += s.substr(pos);
			else
				b += s.substr(pos,len);
		#elseif js
			b[b.length] = s.substr(pos,len);
		#else
			b += s.substr(pos,len);
		#end
	}

	/**
		Adds a character to the string buffer.
	**/
	public inline function addChar( c : Int ) untyped {
		#if neko
			__add_char(b,c);
		#elseif js
			b[b.length] = String.fromCharCode(c);
		#elseif (flash && !flash9)
			b += String["fromCharCode"](c);
		#else
			b += String.fromCharCode(c);
		#end
	}

	/**
		Returns the content of the string buffer.
		The buffer is not emptied by this operation.
	**/
	public inline function toString() : String {
		#if neko
			return new String(__string(b));
		#elseif js
			return b.join("");
		#else
			return b;
		#end
	}

	private var b :
	#if neko
		Dynamic
	#elseif js
		Array<Dynamic>
	#else
		String
	#end;

#if neko
	static var __make : Dynamic = neko.Lib.load("std","buffer_new",0);
	static var __add : Dynamic = neko.Lib.load("std","buffer_add",2);
	static var __add_char : Dynamic = neko.Lib.load("std","buffer_add_char",2);
	static var __add_sub : Dynamic = neko.Lib.load("std","buffer_add_sub",4);
	static var __string : Dynamic = neko.Lib.load("std","buffer_string",1);
#end

}
