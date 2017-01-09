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
package php;

@:native('Error')
extern class Error implements Throwable {
	function new(?message : String, ?code : Int, ?previous:Throwable) : Void;

	private var message : String;
	private var code : Int;
	private var file : String;
	private var line : Int;

	@:final
	function getPrevious() : Throwable;   // Returns previous Throwable
	function getMessage() : String;       // message of the exception
	function getCode() : Int;             // code of the exception
	function getFile() : String;          // source filename
	function getLine() : Int;             // source line
	function getTrace() : NativeIndexedArray<NativeAssocArray<Dynamic>>;  // an array of the backtrace
	function getTraceAsString() : String; // formated string of trace
	@:phpMagic function __toString() : String;       // formated string for display
}