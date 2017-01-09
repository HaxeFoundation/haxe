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

// This file is generated from mozilla\IDBFileHandle.webidl. Do not edit!

package js.html.idb;

@:native("IDBFileHandle")
extern class FileHandle extends js.html.EventTarget
{
	var mutableFile(default,null) : MutableFile;
	var fileHandle(default,null) : MutableFile;
	var mode(default,null) : js.html.FileMode;
	var active(default,null) : Bool;
	var location : Int;
	var oncomplete : haxe.Constraints.Function;
	var onabort : haxe.Constraints.Function;
	var onerror : haxe.Constraints.Function;
	
	/** @throws DOMError */
	function getMetadata( ?parameters : FileMetadataParameters ) : FileRequest;
	/** @throws DOMError */
	function readAsArrayBuffer( size : Int ) : FileRequest;
	/** @throws DOMError */
	function readAsText( size : Int, ?encoding : String ) : FileRequest;
	/** @throws DOMError */
	function write( value : haxe.extern.EitherType<String,haxe.extern.EitherType<js.html.ArrayBuffer,haxe.extern.EitherType<js.html.ArrayBufferView,js.html.Blob>>> ) : FileRequest;
	/** @throws DOMError */
	function append( value : haxe.extern.EitherType<String,haxe.extern.EitherType<js.html.ArrayBuffer,haxe.extern.EitherType<js.html.ArrayBufferView,js.html.Blob>>> ) : FileRequest;
	/** @throws DOMError */
	function truncate( ?size : Int ) : FileRequest;
	/** @throws DOMError */
	function flush() : FileRequest;
	/** @throws DOMError */
	function abort() : Void;
}