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

/**
	@see http://php.net/manual/en/class.errorexception.php
**/
@:native('ErrorException')
extern class ErrorException implements Throwable {
	function new (?message:String, ?code:Int, ?severety:Int, ?filename:String, ?lineno:Int, ?previous:Throwable) : Void;

	@:final function getSeverity() : Int;
	@:final function getPrevious() : Throwable;   // Returns previous Throwable
    @:final function getMessage() : String;       // message of the exception
    @:final function getCode() : Int;             // code of the exception
    @:final function getFile() : String;          // source filename
    @:final function getLine() : Int;             // source line
    @:final function getTrace() : NativeIndexedArray<NativeAssocArray<Dynamic>>;  // an array of the backtrace
    @:final function getTraceAsString() : String; // formated string of trace
	@:phpMagic function __toString() : String;       // formated string for display
}