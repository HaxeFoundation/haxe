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
