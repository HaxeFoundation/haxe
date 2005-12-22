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

class StringBuf {

	public function new() {
		#neko
		b = __make();
		#else flash
		b = "";
		#else error
		#end
	}

	public function add( x : Dynamic ) {
		#neko
		__add(b,x);
		#else flash
		b += x;
		#else error
		#end
	}

	public function addSub( s : String, pos : Int, len : Int ) {
		#neko
		__add_sub(b,untyped s.__s,pos,len);
		#else flash
		b += s.substr(pos,len);
		#else error
		#end
	}

	public function addChar( c : Int ) {
		#neko
		__add_char(b,c);
		#else flash
		b += untyped String.fromCharCode(c);
		#else error
		#end
	}

	public function toString() : String {
		#neko
		return new String(__string(b));
		#else flash
		return b;
		#else error
		#end
	}

	private var b : Dynamic;

#neko
	static var __make : Dynamic = Neko.load("std","buffer_new",0);
	static var __add : Dynamic = Neko.load("std","buffer_add",2);
	static var __add_char : Dynamic = Neko.load("std","buffer_add_char",2);
	static var __add_sub : Dynamic = Neko.load("std","buffer_add_sub",4);
	static var __string : Dynamic = Neko.load("std","buffer_string",1);
#end

}