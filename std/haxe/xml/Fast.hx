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
package haxe.xml;

private class NodeAccess implements Dynamic<Fast> {

	var __x : Xml;

	public function new( x : Xml ) {
		__x = x;
	}

	public function resolve( name : String ) : Fast {
		var x = __x.elementsNamed(name).next();
		if( x == null ) {
			var xname = if( __x.nodeType == Xml.Document ) "Document" else __x.nodeName;
			throw xname+" is missing element "+name;
		}
		return new Fast(x);
	}

}

private class AttribAccess implements Dynamic<String> {

	var __x : Xml;

	public function new( x : Xml ) {
		__x = x;
	}

	public function resolve( name : String ) : String {
		if( __x.nodeType == Xml.Document )
			throw "Cannot access document attribute "+name;
		var v = __x.get(name);
		if( v == null )
			throw __x.nodeName+" is missing attribute "+name;
		return v;
	}

}

private class HasAttribAccess implements Dynamic<Bool> {

	var __x : Xml;

	public function new( x : Xml ) {
		__x = x;
	}

	public function resolve( name : String ) : Bool {
		if( __x.nodeType == Xml.Document )
			throw "Cannot access document attribute "+name;
		return __x.exists(name);
	}

}

private class HasNodeAccess implements Dynamic<Bool> {

	var __x : Xml;

	public function new( x : Xml ) {
		__x = x;
	}

	public function resolve( name : String ) : Bool {
		return __x.elementsNamed(name).hasNext();
	}

}

private class NodeListAccess implements Dynamic<List<Fast>> {

	var __x : Xml;

	public function new( x : Xml ) {
		__x = x;
	}

	public function resolve( name : String ) : List<Fast> {
		var l = new List();
		for( x in __x.elementsNamed(name) )
			l.add(new Fast(x));
		return l;
	}

}

/**
	The `haxe.xml.Fast` API helps providing a fast dot-syntax access to the
	most common `Xml` methods.
**/
class Fast {
	/**
		The current corresponding `Xml` node.
	**/
	public var x(default, null) : Xml;

	/**
		The name of the current element. This is the same as `Xml.nodeName`.
	**/
	public var name(get,null) : String;

	/**
		The inner PCDATA or CDATA of the node.

		An exception is thrown if there is no data or if there not only data
		but also other nodes.
	**/
	public var innerData(get,null) : String;

	/**
		The XML string built with all the sub nodes, excluding the current one.
	**/
	public var innerHTML(get,null) : String;

	/**
		Access to the first sub element with the given name.

		An exception is thrown if the element doesn't exists.
		Use `hasNode` to check the existence of a node.

		```haxe
		var fast = new haxe.xml.Fast(Xml.parse("<user><name>John</name></user>"));
		var user = fast.node.user;
		var name = user.node.name;
		trace(name.innerData); // John

		// Uncaught Error: Document is missing element password
		var password = user.node.password;
		```
	**/
	public var node(default,null) : NodeAccess;

	/**
		Access to the List of elements with the given name.
		```haxe
		var fast = new haxe.xml.Fast(Xml.parse("<users>
				<user name='John'/>
				<user name='Andy'/>
				<user name='Dan'/>
		</users>"));

		var users = fast.node.users;
		for(user in users.nodes.user) {
				trace(user.att.name);
		}
		```
	**/
	public var nodes(default,null) : NodeListAccess;

	/**
		Access to a given attribute.

		An exception is thrown if the attribute doesn't exists.
		Use `has` to check the existence of an attribute.

		```haxe
		var f = new haxe.xml.Fast(Xml.parse("<user name='Mark'></user>"));
		var user = f.node.user;
		if (user.has.name) {
			trace(user.att.name); // Mark
		}
		```
	**/
	public var att(default,null) : AttribAccess;

	/**
		Check the existence of an attribute with the given name.
	**/
	public var has(default,null) : HasAttribAccess;

	/**
		Check the existence of a sub node with the given name.

		```haxe
		var f = new haxe.xml.Fast(Xml.parse("<user><age>31</age></user>"));
		var user = f.node.user;
		if (user.hasNode.age) {
			trace(user.node.age.innerData); // 31
		}
		```
	**/
	public var hasNode(default,null) : HasNodeAccess;

	/**
		The list of all sub-elements which are the nodes with type `Xml.Element`.
	**/
	public var elements(get,null) : Iterator<Fast>;

	public function new( x : Xml ) {
		if( x.nodeType != Xml.Document && x.nodeType != Xml.Element )
			throw "Invalid nodeType "+x.nodeType;
		this.x = x;
		node = new NodeAccess(x);
		nodes = new NodeListAccess(x);
		att = new AttribAccess(x);
		has = new HasAttribAccess(x);
		hasNode = new HasNodeAccess(x);
	}

	function get_name() {
		return if( x.nodeType == Xml.Document ) "Document" else x.nodeName;
	}

	function get_innerData() {
		var it = x.iterator();
		if( !it.hasNext() )
			throw name+" does not have data";
		var v = it.next();
		if( it.hasNext() ) {
			var n = it.next();
			// handle <spaces>CDATA<spaces>
			if( v.nodeType == Xml.PCData && n.nodeType == Xml.CData && StringTools.trim(v.nodeValue) == "" ) {
				if( !it.hasNext() )
					return n.nodeValue;
				var n2 = it.next();
				if( n2.nodeType == Xml.PCData && StringTools.trim(n2.nodeValue) == "" && !it.hasNext() )
					return n.nodeValue;
			}
			throw name+" does not only have data";
		}
		if( v.nodeType != Xml.PCData && v.nodeType != Xml.CData )
			throw name+" does not have data";
		return v.nodeValue;
	}

	function get_innerHTML() {
		var s = new StringBuf();
		for( x in x )
			s.add(x.toString());
		return s.toString();
	}

	function get_elements() {
		var it = x.elements();
		return {
			hasNext : it.hasNext,
			next : function() {
				var x = it.next();
				if( x == null )
					return null;
				return new Fast(x);
			}
		};
	}
}
