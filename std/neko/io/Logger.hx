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
package neko.io;

class Logger extends Input {

	var input : Input;
	var output : Output;
	var autoFlush : Bool;

	public function new(i,?o,?flush) {
		input = i;
		output = o;
		autoFlush = if( flush == null ) output != null else flush;
	}

	public function logChar( c : Int ) {
	}

	public function logString( buf : String ) {
	}

	public override function readChar() {
		var c = input.readChar();
		logChar(c);
		if( output != null ) output.writeChar(c);
		if( autoFlush ) output.flush();
		return c;
	}

	public override function readBytes(buf,pos,len) {
		var n = input.readBytes(buf,pos,len);
		logString(buf.substr(pos,n));
		if( output != null ) output.writeBytes(buf,pos,n);
		if( autoFlush ) output.flush();
		return n;
	}

	public override function close() {
		input.close();
		if( output != null ) output.close();
	}

}
