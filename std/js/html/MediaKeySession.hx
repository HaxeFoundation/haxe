/*
 * Copyright (C)2005-2015 Haxe Foundation
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

// This file is generated from mozilla/MediaKeySession.webidl line 14:0. Do not edit!

package js.html;

@:native("MediaKeySession")
extern class MediaKeySession extends EventTarget
{
	var error(default,null) : MediaKeyError;
	var keySystem(default,null) : String;
	var sessionId(default,null) : String;
	var expiration(default,null) : Float;
	var closed(default,null) : Promise<Void>;
	
	function generateRequest( initDataType : String, initData : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer> ) : Promise<Void>;
	function load( sessionId : String ) : Promise<Bool>;
	function update( response : haxe.extern.EitherType<ArrayBufferView,ArrayBuffer> ) : Promise<Void>;
	function close() : Promise<Void>;
	function remove() : Promise<Void>;
	function getUsableKeyIds() : Promise<Array<ArrayBuffer>>;
}