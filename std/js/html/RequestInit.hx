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

// This file is generated from mozilla\Request.webidl. Do not edit!

package js.html;

typedef RequestInit =
{
	@:optional var body : haxe.extern.EitherType<ArrayBuffer,haxe.extern.EitherType<ArrayBufferView,haxe.extern.EitherType<Blob,haxe.extern.EitherType<FormData,haxe.extern.EitherType<String,URLSearchParams>>>>>;
	@:optional var cache : RequestCache;
	@:optional var credentials : RequestCredentials;
	@:optional var headers : haxe.extern.EitherType<Headers,haxe.extern.EitherType<Array<Array<String>>,Dynamic/*MISSING ByteStringMozMap*/>>;
	@:optional var method : String;
	@:optional var mode : RequestMode;
	@:optional var redirect : RequestRedirect;
	@:optional var referrer : String;
	@:optional var referrerPolicy : ReferrerPolicy;
}