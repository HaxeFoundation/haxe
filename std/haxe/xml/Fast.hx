/*
 * Copyright (c) 2005-2007, The haXe Project Contributors
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
package haxe.xml;

private class NodeAccess implements Dynamic<Fast> {

	var __x : Xml;

	public function new( x : Xml ) {
		__x = x;
	}

	function __resolve( name : String ) : Fast {
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

	function __resolve( name : String ) : String {
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

	function __resolve( name : String ) : Bool {
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

	function __resolve( name : String ) : Bool {
		return __x.elementsNamed(name).hasNext();
	}

}

private class NodeListAccess implements Dynamic<List<Fast>> {

	var __x : Xml;

	public function new( x : Xml ) {
		__x = x;
	}

	function __resolve( name : String ) : List<Fast> {
		var l = new List();
		for( x in __x.elementsNamed(name) )
			l.add(new Fast(x));
		return l;
	}

}

class Fast {

	public var x(default,null) : Xml;
	public var name(getName,null) : String;
	public var innerData(getInnerData,null) : String;
	public var innerHTML(getInnerHTML,null) : String;
	public var node(default,null) : NodeAccess;
	public var nodes(default,null) : NodeListAccess;
	public var att(default,null) : AttribAccess;
	public var has(default,null) : HasAttribAccess;
	public var hasNode(default,null) : HasNodeAccess;
	public var elements(getElements,null) : Iterator<Fast>;

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

	function getName() {
		return if( x.nodeType == Xml.Document ) "Document" else x.nodeName;
	}

	function getInnerData() {
		var it = x.iterator();
		if( !it.hasNext() )
			throw name+" does not have data";
		var v = it.next();
		if( it.hasNext() )
			throw name+" does not only have data";
		if( v.nodeType != Xml.PCData && v.nodeType != Xml.CData )
			throw name+" does not have data";
		return v.nodeValue;
	}

	function getInnerHTML() {
		var s = new StringBuf();
		for( x in x )
			s.add(x.toString());
		return s.toString();
	}

	function getElements() {
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
