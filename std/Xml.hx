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
	The standard Xml class and parsing.
	More API to manipulate XML are available in the [haxe.xml] package.
**/

extern class Xml {

	/**
		A type of Xml node.
	**/
	static var Element(default,null) : XmlType;

	/**
		A type of Xml node.
	**/
	static var PCData(default,null) : XmlType;

	/**
		A type of Xml node.
	**/
	static var CData(default,null) : XmlType;

	/**
		A type of Xml node.
	**/
	static var Comment(default,null) : XmlType;

	/**
		A type of Xml node.
	**/
	static var DocType(default,null) : XmlType;

	/**
		A type of Xml node.
	**/
	static var Prolog(default,null) : XmlType;

	/**
		A type of Xml node.
	**/
	static var Document(default,null) : XmlType;

	/**
		Parse a String into an Xml object.
	**/
	static function parse( str : String ) : Xml;

	/**
		Creates a node of the given type.
	**/
	static function createElement( name : String ) : Xml;

	/**
		Creates a node of the given type.
	**/
	static function createPCData( data : String ) : Xml;

	/**
		Creates a node of the given type.
	**/
	static function createCData( data : String ) : Xml;

	/**
		Creates a node of the given type.
	**/
	static function createComment( data : String ) : Xml;

	/**
		Creates a node of the given type.
	**/
	static function createDocType( data : String ) : Xml;

	/**
		Creates a node of the given type.
	**/
	static function createProlog( data : String ) : Xml;

	/**
		Creates a node of the given type.
	**/
	static function createDocument() : Xml;

	/**
		Returns the type of the Xml Node. This should be used before
		accessing other functions since some might raise an exception
		if the node type is not correct.
	**/
	var nodeType(default,null) : XmlType;

	/**
		Returns the node name of an Element.
	**/
	var nodeName(get_nodeName,set_nodeName) : String;
	private function get_nodeName() : String;
	private function set_nodeName( name : String ) : String;

	/**
		Returns the node value. Only works if the Xml node is not an Element or a Document.
	**/
	var nodeValue(get_nodeValue,set_nodeValue) : String;
	private function get_nodeValue() : String;
	private function set_nodeValue( name : String ) : String;

	/**
		Get the given attribute of an Element node. Returns [null] if not found.
		Attributes are case-sensitive.
	**/
	function get( att : String ) : String; // check case insensitivy

	/**
		Set the given attribute value for an Element node.
		Attributes are case-sensitive.
	**/
	function set( att : String, value : String ) : Void;

	/**
		Removes an attribute for an Element node.
		Attributes are case-sensitive.
	**/
	function remove( att : String ) : Void;

	/**
		Tells if the Element node has a given attribute.
		Attributes are case-sensitive.
	**/
	function exists( att : String ) : Bool;

	/**
		Returns an [Iterator] on all the attribute names.
	**/
	function attributes() : Iterator<String>;

	/**
		Returns the parent object in the Xml hierarchy.
		The parent can be [null], an Element or a Document.
	**/
	var parent(getParent,null) : Xml;
	private function getParent() : Xml;

	/**
		Returns an iterator of all child nodes.
		Only works if the current node is an Element or a Document.
	**/
	function iterator() : Iterator<Xml>;

	/**
		Returns an iterator of all child nodes which are Elements.
		Only works if the current node is an Element or a Document.
	**/
	function elements() : Iterator<Xml>;

	/**
		Returns an iterator of all child nodes which are Elements with the given nodeName.
		Only works if the current node is an Element or a Document.
	**/
	function elementsNamed( name : String ) : Iterator<Xml>;

	/**
		Returns the first child node.
	**/
	function firstChild() : Xml;

	/**
		Returns the first child node which is an Element.
	**/
	function firstElement() : Xml;


	/**
		Adds a child node to the Document or Element.
		One node can only be inside one given node which is indicated by the [parent] property.
	**/
	function addChild( x : Xml ) : Void;

	/**
		Removes a child from the Document or Element.
		Returns true if the child was successfuly removed.
	**/
	function removeChild( x : Xml ) : Bool;

	/**
		Inserts a child at the given position among the other childs.
	**/
	function insertChild( x : Xml, pos : Int ) : Void;

	/**
		Returns a String representation of the Xml node.
	**/
	function toString() : String;

}





