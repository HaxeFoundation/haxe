/*
 * Copyright (C)2005-2012 Haxe Foundation
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
package js;

@:initPackage
extern class XMLHttpRequest {

	var onreadystatechange : Void -> Void;
	var readyState : Int;
	var responseText : String;
	//var responseXML : Xml;
	var status : Int;
	var statusText : String;

	function new() : Void;

	function abort() : Void;

	function getAllResponseHeaders() : String;
	function getResponseHeader( name : String ) : String;
	function setRequestHeader( name : String, value : String ) : Void;
	function open( method : String, url : String, async : Bool ) : Void;
	function send( content : String ) : Void;

	#if !jsfl
	private static function __init__() : Void {
		untyped
		js["XMLHttpRequest"] =
			if( window.XMLHttpRequest )
				__js__("XMLHttpRequest");
			else if( window.ActiveXObject )
				function() {
					try {
						return __new__("ActiveXObject","Msxml2.XMLHTTP");
					}catch(e:Dynamic){
						try {
							return __new__("ActiveXObject","Microsoft.XMLHTTP");
						}catch(e:Dynamic){
							throw "Unable to create XMLHttpRequest object.";
						}
					}
				};
			else
				throw "Unable to create XMLHttpRequest object.";
	}
	#end

}
