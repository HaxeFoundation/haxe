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
package cpp;

import Xml;

class CppXml__ {
	public static var Element(default,null) : XmlType;
	public static var PCData(default,null) : XmlType;
	public static var CData(default,null) : XmlType;
	public static var Comment(default,null) : XmlType;
	public static var DocType(default,null) : XmlType;
	public static var Prolog(default,null) : XmlType;
	public static var Document(default,null) : XmlType;


	private var _nodeName : String;
	private var _nodeValue : String;
	private var _attributes : Dynamic<String>;
	private var _children : Array<Dynamic>;
	private var _parent : CppXml__;

	private function new() {
	}

	private static var _parse = cpp.Lib.load("std","parse_xml",2);

	public static function parse( xmlData : String ) : CppXml__ {
		var x = new CppXml__();
		x._children = new Array();
		var parser = {
			cur : x,
			xml : function(name,att) {
				var x : Dynamic = new CppXml__();
				x._parent = untyped this.cur;
				x.nodeType = Xml.Element;
				x._nodeName = new String(name);
				x._attributes = att;
				x._children = new Array();
				untyped {
					var i = 0;
					this.cur.addChild(x);
					this.cur = x;
				}
			},
			cdata : function(text) {
				var x = new CppXml__();
				x._parent = untyped this.cur;
				x.nodeType = Xml.CData;
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			pcdata : function(text) {
				var x = new CppXml__();
				x._parent = untyped this.cur;
				x.nodeType = Xml.PCData;
				x._nodeValue = new String(text);
				untyped this.cur.addChild(x);
			},
			comment : function(text:String) {
				var x = new CppXml__();
				x._parent = untyped this.cur;
				if( untyped text.cca(0) == 63 ) {
					x.nodeType = Xml.Prolog;
					text = "<"+new String(text)+">";
				} else {
					x.nodeType = Xml.Comment;
					text = "<!--"+new String(text)+"-->";
				}
				x._nodeValue = text;
				untyped this.cur.addChild(x);
			},
			doctype : function(text) {
				var x = new CppXml__();
				x._parent = untyped this.cur;
				x.nodeType = Xml.DocType;
				x._nodeValue = "<!DOCTYPE"+new String(text)+">";
				untyped this.cur.addChild(x);
			},
			done : function() {
				untyped this.cur = this.cur._parent;
			}
		};
		untyped _parse(xmlData,parser);
		x.nodeType = Xml.Document;
		return x;
	}


	public static function createElement( name : String ) : CppXml__ {
		var r = new CppXml__();
		r.nodeType = Xml.Element;
		r._nodeName = name;
		r._attributes = null;
		r._children = new Array();
		return r;
	}

	public static function createPCData( data : String ) : CppXml__ {
		var r = new CppXml__();
		r.nodeType = Xml.PCData;
		r._nodeValue = data;
		return r;
	}

	public static function createCData( data : String ) : CppXml__ {
		var r = new CppXml__();
		r.nodeType = Xml.CData;
		r._nodeValue = data;
		return r;
	}

	public static function createComment( data : String ) : CppXml__ {
		var r = new CppXml__();
		r.nodeType = Xml.Comment;
		r._nodeValue = data;
		return r;
	}

	public static function createDocType( data : String ) : CppXml__ {
		var r = new CppXml__();
		r.nodeType = Xml.DocType;
		r._nodeValue = data;
		return r;
	}

	public static function createProlog( data : String ) : CppXml__ {
		var r = new CppXml__();
		r.nodeType = Xml.Prolog;
		r._nodeValue = data;
		return r;
	}

	public static function createDocument() : CppXml__ {
		var r = new CppXml__();
		r.nodeType = Xml.Document;
		r._children = new Array();
		return r;
	}

	/**
		Returns the type of the Xml Node. This should be used before
		accessing other functions since some might raise an exception
		if the node type is not correct.
	**/
	public var nodeType(default,null) : XmlType;

	/**
		Returns the node name of an Element.
	**/
	public var nodeName(getNodeName,setNodeName) : String;

	/**
		Returns the node value. Only works if the Xml node is not an Element or a Document.
	**/
	public var nodeValue(getNodeValue,setNodeValue) : String;


	private function getNodeName() : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _nodeName;
	}

	private function setNodeName( n : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _nodeName = n;
	}

	private function getNodeValue() : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return _nodeValue;
	}

	private function setNodeValue( v : String ) : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return _nodeValue = v;
	}

	private function getParent() : Xml {
		return _parent;
	}

	public function get( att : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.field( _attributes, att );
	}

	public function set( att : String, value : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		Reflect.setField (_attributes, att, value );
		return null;
	}

	public function remove( att : String ) : Void{
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		Reflect.deleteField( _attributes, att );
		return null;
	}

	public function exists( att : String ) : Bool {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.hasField( _attributes, att );
	}

	public function attributes() : Iterator<String> {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return Reflect.fields( _attributes ).iterator();
	}

	public function iterator() : Iterator<Xml> {
		if( _children == null )
			throw "bad nodetype";
      return untyped _children.iterator();
	}


	public function elements(): Iterator<Xml> {
		if( _children == null )
			throw "bad nodetype";
      var children = _children;
		return untyped {
			cur: 0,
			hasNext : function() {
				var k:Int = this.cur;
				var l = children.length;
				while( k < l ) {
					if( children[k].nodeType == Xml.Element )
						break;
					k += 1;
				}
				this.cur = k;
				return k < l;
			},
			next : function() {
				var k = this.cur;
				var l = children.length;
				while( k < l ) {
					var n = children[k];
					k += 1;
					if( n.nodeType == Xml.Element ) {
						this.cur = k;
						return n;
					}
				}
				return null;
			}
		}
	}

	public function elementsNamed( name : String ) : Iterator<Xml> {
		if( _children == null )
			throw "bad nodetype";
      var children = _children;
		return untyped {
			cur: 0,
			hasNext : function() {
				var k = this.cur;
				var l = children.length;
				while( k < l ) {
					var n = children[k];
					if( n.nodeType == Xml.Element && n._nodeName == name )
						break;
					k++;
				}
				this.cur = k;
				return k < l;
			},
			next : function() {
				var k = this.cur;
				var l = children.length;
				while( k < l ) {
					var n = children[k];
					k++;
					if( n.nodeType == Xml.Element && n._nodeName == name ) {
						this.cur = k;
						return n;
					}
				}
				return null;
			}
		}
	}

	public function firstChild() : Xml {
		if( _children == null )
			throw "bad nodetype";
		return _children[0];
	}

	public function firstElement() : Xml {
		if( _children == null )
			throw "bad nodetype";
		for( cur in 0..._children.length ) {
			var n:CppXml__ = _children[cur];
			if( n.nodeType == Xml.Element )
				return n;
		}
		return null;
	}

   public function addChild( x_ : Xml ) : Void {
      var x:CppXml__ = cast x_;
		if( _children == null )
			throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.push( x );
		return null;
	}

   public function removeChild( x_ : Xml ) : Bool {
      var x:CppXml__ = cast x_;
		if( _children == null )
			throw "bad nodetype";
		var b = _children.remove( x );
		if( b ) x._parent = null;
		return b;
	}

	public function insertChild( x_ : Xml, pos : Int ) : Void {
      var x:CppXml__ = cast x_;
		if( _children == null )
			throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.insert( pos, x );
		return null;
	}

	public function toString() {
		if( nodeType == Xml.PCData )
			return _nodeValue;
		if( nodeType == Xml.CData )
			return "<![CDATA["+_nodeValue+"]]>";
		if( nodeType == Xml.Comment || nodeType == Xml.DocType || nodeType == Xml.Prolog )
			return _nodeValue;

		var s = new StringBuf();

		if( nodeType == Xml.Element ) {
			s.add("<");
			s.add(_nodeName);
			for( k in Reflect.fields(_attributes) ) {
				s.add(" ");
				s.add(k);
				s.add("=\"");
				s.add(Reflect.field(_attributes,k));
				s.add("\"");
			}
			if( _children.length == 0 ) {
				s.add("/>");
				return s.toString();
			}
			s.add(">");
		}
		for( x in iterator() )
			s.add(x);
		if( nodeType == Xml.Element ) {
			s.add("</");
			s.add(_nodeName);
			s.add(">");
		}
		return s.toString();
	}

	static function __init__() : Void {
		Element = "element";
		PCData = "pcdata";
		CData = "cdata";
		Comment = "comment";
		DocType = "doctype";
		Prolog = "prolog";
		Document = "document";
	}
}

