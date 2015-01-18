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

// This file is generated from mozilla/DeviceStorage.webidl line 10:0. Do not edit!

package js.html;

@:native("DeviceStorage")
extern class DeviceStorage extends EventTarget
{
	var onchange : haxe.Constraints.Function;
	var storageName(default,null) : String;
	var canBeMounted(default,null) : Bool;
	var canBeShared(default,null) : Bool;
	var canBeFormatted(default,null) : Bool;
	@:native("default")
	var default_(default,null) : Bool;
	
	/** @throws DOMError */
	function add( aBlob : Blob ) : DOMRequest;
	/** @throws DOMError */
	function addNamed( aBlob : Blob, aName : String ) : DOMRequest;
	/** @throws DOMError */
	function appendNamed( aBlob : Blob, aName : String ) : DOMRequest;
	/** @throws DOMError */
	function get( aName : String ) : DOMRequest;
	/** @throws DOMError */
	function getEditable( aName : String ) : DOMRequest;
	/** @throws DOMError */
	@:native("delete")
	function delete_( aName : String ) : DOMRequest;
	/** @throws DOMError */
	@:overload( function( ?options : DeviceStorageEnumerationParameters ) : DOMCursor {} )
	function enumerate( path : String, ?options : DeviceStorageEnumerationParameters ) : DOMCursor;
	/** @throws DOMError */
	@:overload( function( ?options : DeviceStorageEnumerationParameters ) : DOMCursor {} )
	function enumerateEditable( path : String, ?options : DeviceStorageEnumerationParameters ) : DOMCursor;
	/** @throws DOMError */
	function freeSpace() : DOMRequest;
	/** @throws DOMError */
	function usedSpace() : DOMRequest;
	/** @throws DOMError */
	function available() : DOMRequest;
	/** @throws DOMError */
	function storageStatus() : DOMRequest;
	/** @throws DOMError */
	function format() : DOMRequest;
	/** @throws DOMError */
	function mount() : DOMRequest;
	/** @throws DOMError */
	function unmount() : DOMRequest;
	/** @throws DOMError */
	function getRoot() : Promise/*<Any>*/;
}