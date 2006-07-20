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
	An abstract type representing the type of the Xml
	Node. You can compare it to [Xml] statics and can
	use [Std.string(t)] to get a string reprensation
	of the type.
**/
enum XmlType {
}

/**
	Xml class and parsing.
**/
extern class Xml {

	static var Element(default,null) : XmlType;
	static var PCData(default,null) : XmlType;
	static var CData(default,null) : XmlType;
	static var Comment(default,null) : XmlType;
	static var DocType(default,null) : XmlType;
	static var Prolog(default,null) : XmlType;
	static var Document(default,null) : XmlType;

	static function parse( s : String ) : Xml;

	static function createElement( name : String ) : Xml;
	static function createPCData( data : String ) : Xml;
	static function createCData( data : String ) : Xml;
	static function createComment( data : String ) : Xml;
	static function createDocType( data : String ) : Xml;
	static function createProlog( data : String ) : Xml;
	static function createDocument() : Xml;

	var nodeType(default,null) : XmlType;

	// nodeName : only works for Node
	var nodeName(getNodeName,setNodeName) : String;
	private function getNodeName() : String;
	private function setNodeName( name : String ) : String;

	// nodeValue : only works for not Node and not Document
	var nodeValue(getNodeValue,setNodeValue) : String;
	private function getNodeValue() : String;
	private function setNodeValue( name : String ) : String;

	// attributes : only works for Node
	function get( att : String ) : String; // check case insensitivy
	function set( att : String, value : String ) : Void;
	function remove( att : String ) : Void;
	function exists( att : String ) : Bool;
	function attributes() : Iterator<String>;

	// children method : only works for Node and Document
	// exception if child is Document (can't add Documents together)
	function iterator() : Iterator<Xml>;
	function elements() : Iterator<Xml>;
	function elementsNamed( name : String ) : Iterator<Xml>; // only nodes with this nodeName
	function firstChild() : Xml;
	function firstElement() : Xml;
	function addChild( x : Xml ) : Void;
	function removeChild( x : Xml ) : Bool;
	function insertChild( x : Xml, pos : Int ) : Void;

	function toString() : String;

	static function __init__() : Void untyped {
		#if neko
			untyped Xml = neko.NekoXml__;
		#else js
			untyped Xml = js.JsXml__;
		#else flash
			untyped Xml = flash.FlashXml__;
		#else error
		#end

		Xml.Element = "element";
		Xml.PCData = "pcdata";
		Xml.CData = "cdata";
		Xml.Comment = "comment";
		Xml.DocType = "doctype";
		Xml.Prolog = "prolog";
		Xml.Document = "document";
	}

}
