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

	var b:String = "";
	
	/**
		Creates a new string buffer.
	**/
	public function new() {}

	/**
		Adds the representation of any value to the string buffer.
	**/
	public inline function add( x : Dynamic ) : Void {
		b += x;
	}

	/**
		Adds a part of a string to the string buffer.
	**/
	public inline function addChar( c : Int ) : Void {
		b += String.fromCharCode(c);
	}

	/**
		Adds a character to the string buffer.
	**/
	public inline function addSub( s : String, pos : Int, ?len : Int) : Void {
		b += s.substr(pos, len);
	}

	/**
		Returns the content of the string buffer.
		The buffer is not emptied by this operation.
	**/
	public inline function toString() : String {
		return b;
	}

}
