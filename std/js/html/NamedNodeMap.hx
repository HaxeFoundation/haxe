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

// This file is generated from mozilla\NamedNodeMap.webidl. Do not edit!

package js.html;

/**
	The `NamedNodeMap` interface represents a collection of `Attr` objects. Objects inside a `NamedNodeMap` are not in any particular order, unlike `NodeList`, although they may be accessed by an index as in an array.

	Documentation [NamedNodeMap](https://developer.mozilla.org/en-US/docs/Web/API/NamedNodeMap) by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/Web/API/NamedNodeMap$history), licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

	@see <https://developer.mozilla.org/en-US/docs/Web/API/NamedNodeMap>
**/
@:native("NamedNodeMap")
extern class NamedNodeMap implements ArrayAccess<Attr>
{
	
	/**
		Returns the amount of objects in the map.
	**/
	var length(default,null) : Int;
	
	
	/**
		Returns a `Attr`, corresponding to the given name.
	**/
	function getNamedItem( name : String ) : Attr;
	/** @throws DOMError */
	
	/**
		Replaces, or adds, the `Attr` identified in the map by the given name.
	**/
	function setNamedItem( arg : Attr ) : Attr;
	/** @throws DOMError */
	
	/**
		Removes the `Attr` identified by the given map.
	**/
	function removeNamedItem( name : String ) : Attr;
	
	/**
		Returns the `Attr` at the given index, or `null` if the index is higher or equal to the number of nodes.
	**/
	function item( index : Int ) : Attr;
	
	/**
		Returns a `Attr` identified by a namespace and related local name.
	**/
	function getNamedItemNS( namespaceURI : String, localName : String ) : Attr;
	/** @throws DOMError */
	
	/**
		Replaces, or adds, the `Attr` identified in the map by the given namespace and related local name.
	**/
	function setNamedItemNS( arg : Attr ) : Attr;
	/** @throws DOMError */
	
	/**
		Removes the `Attr` identified by the given namespace and related local name.
	**/
	function removeNamedItemNS( namespaceURI : String, localName : String ) : Attr;
}