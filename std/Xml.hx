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
	static var ProcessingInstruction(default,null) : XmlType;

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
	static function createProcessingInstruction( data : String ) : Xml;

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
	var nodeName(get,set) : String;

	/**
		Returns the node value. Only works if the Xml node is not an Element or a Document.
	**/
	var nodeValue(get,set) : String;

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
	var parent(get,null) : Xml;

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