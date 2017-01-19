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

import php.Lib;

enum XmlType {
}

typedef NativeXml = Xml;

class Xml {

	public static var Element(default,null) : XmlType;
	public static var PCData(default,null) : XmlType;
	public static var CData(default,null) : XmlType;
	public static var Comment(default,null) : XmlType;
	public static var DocType(default,null) : XmlType;
	public static var ProcessingInstruction(default,null) : XmlType;
	public static var Document(default,null) : XmlType;

	public var nodeType(default,null) : XmlType;
	public var nodeName(get,set) : String;
	public var nodeValue(get,set) : String;
	public var parent(get,null) : Xml;

	var _nodeName : String;
	var _nodeValue : String;
	var _attributes : haxe.ds.StringMap<String>;
	var _children : Array<Xml>;
	var _parent : Xml;
	var _fromCustomParser:Bool;

	private static var build : Xml;
	private static function __start_element_handler(parser : Dynamic, name : String, attribs : ArrayAccess<String>) : Void {
		var node = createElement(name);
		untyped __php__("foreach($attribs as $k => $v) $node->set($k, $v)");
		build.addChild(node);
		build = node;
	}

	private static function __end_element_handler(parser : Dynamic, name : String) : Void {
		build = build.parent;
	}

	private static function __decodeattr(value : String) : String
	{
		return untyped __call__("str_replace", "'", '&apos;', __call__("htmlspecialchars", value, __php__('ENT_COMPAT'), 'UTF-8'));
	}

	private static function __decodeent(value : String) : String
	{
		return untyped __call__("str_replace", "'", '&apos;', __call__("htmlentities", value, __php__('ENT_COMPAT'), 'UTF-8'));
	}

	private static function __character_data_handler(parser : Dynamic, data : String) : Void {
		var d = __decodeent(data);
		if ((untyped __call__("strlen", data) == 1 && d != data) || d == data) {
			var last = build._children[build._children.length - 1];
			if (null != last && last.nodeType == Xml.PCData)
			{
				last.nodeValue += d;
			} else
				build.addChild(createPCData(d));
		} else {
			build.addChild(createCData(data));
		}
	}

	private static function __default_handler(parser : Dynamic, data : String) : Void {
		//On some PHP setups (seems to happen when libexpat is used) we may get called for such "entities" although character_data will correctly be called afterward.
		if(data == "<![CDATA[")
			return;
		if(data == "]]>")
			return;
		if ("<!--" == data.substr(0, 4))
			build.addChild(createComment(data.substr(4, data.length-7)));
		else
			build.addChild(createPCData(data));
	}

	static var reHeader = ~/\s*(?:<\?(.+?)\?>)?(?:<!DOCTYPE ([^>]+)>)?/mi;

	public static function parse( str : String ) : Xml {
		build = createDocument();
		var xml_parser = untyped __call__("xml_parser_create");
		untyped __call__("xml_set_element_handler", xml_parser, __start_element_handler, __end_element_handler);
		untyped __call__("xml_set_character_data_handler", xml_parser, __character_data_handler);
		untyped __call__("xml_set_default_handler", xml_parser, __default_handler);
		untyped __call__("xml_parser_set_option", xml_parser, __php__("XML_OPTION_CASE_FOLDING"), 0);
		untyped __call__("xml_parser_set_option", xml_parser, __php__("XML_OPTION_SKIP_WHITE"), 0);

		reHeader.match(str);

		str = "<doc>"+reHeader.matchedRight()+"</doc>";

		if(1 != untyped __call__("xml_parse", xml_parser, str, true)) {
			throw "Xml parse error ("+untyped __call__("xml_error_string", __call__("xml_get_error_code", xml_parser)) + ") line #" + __call__("xml_get_current_line_number", xml_parser);
		}

		untyped __call__("xml_parser_free", xml_parser);

		build = build._children[0];
		build._parent = null;
		build._nodeName = null;
		build.nodeType = Document;

		var doctype = reHeader.matched(2);
		if (null != doctype)
			build.insertChild(createDocType(doctype), 0);

		var ProcessingInstruction = reHeader.matched(1);
		if (null != ProcessingInstruction)
			build.insertChild(createProcessingInstruction(ProcessingInstruction), 0);

		return build;
	}

	private function new(fromCustomParser:Bool = false) : Void {
		_fromCustomParser = fromCustomParser;
	}

	@:allow(haxe.xml.Parser)
	static function createPCDataFromCustomParser( data : String ) : Xml {
		var r = new Xml(true);
		r.nodeType = Xml.PCData;
		r.set_nodeValue( data );
		return r;
	}

	public static function createElement( name : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.Element;
		r._children = new Array();
		r._attributes = new haxe.ds.StringMap();
		r.set_nodeName( name );
		return r;
	}

	public static function createPCData( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.PCData;
		r.set_nodeValue( data );
		return r;
	}

	public static function createCData( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.CData;
		r.set_nodeValue( data );
		return r;
	}

	public static function createComment( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.Comment;
		r.set_nodeValue( data );
		return r;
	}

	public static function createDocType( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.DocType;
		r.set_nodeValue( data );
		return r;
	}

	public static function createProcessingInstruction( data : String ) : Xml {
		var r = new Xml();
		r.nodeType = Xml.ProcessingInstruction;
		r.set_nodeValue( data );
		return r;
	}

	public static function createDocument() : Xml {
		var r = new Xml();
		r.nodeType = Xml.Document;
		r._children = new Array();
		return r;
	}

	private function get_nodeName() : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _nodeName;
	}

	private function set_nodeName( n : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _nodeName = n;
	}

	private function get_nodeValue() : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return _nodeValue;
	}

	private function set_nodeValue( v : String ) : String {
		if( nodeType == Xml.Element || nodeType == Xml.Document )
			throw "bad nodeType";
		return _nodeValue = v;
	}

	private inline function get_parent() : Xml {
		return _parent;
	}

	public function get( att : String ) : String {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _attributes.get( att );
	}

	// TODO: check correct transform function
	@:ifFeature("Xml.parse")
	public function set( att : String, value : String ) : Void {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		_attributes.set( att, __decodeattr(value) );
	}

	public function remove( att : String ) : Void{
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		_attributes.remove( att );
	}

	public function exists( att : String ) : Bool {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _attributes.exists( att );
	}

	public function attributes() : Iterator<String> {
		if( nodeType != Xml.Element )
			throw "bad nodeType";
		return _attributes.keys();
	}

	public function iterator() : Iterator<Xml> {
		if( _children == null ) throw "bad nodetype";
		return _children.iterator();
	}

	public function elements() : Iterator<Xml> {
		if( _children == null ) throw "bad nodetype";
		return Lambda.filter(_children, function(child) return child.nodeType == Xml.Element).iterator();
	}

	public function elementsNamed( name : String ) : Iterator<Xml> {
		if( _children == null ) throw "bad nodetype";
		return Lambda.filter(_children, function(child) return child.nodeType == Xml.Element && child.nodeName == name).iterator();
	}

	public function firstChild() : Xml {
		if( _children == null ) throw "bad nodetype";
		if( _children.length == 0 ) return null;
		return _children[0];
	}

	public function firstElement() : Xml {
		if( _children == null ) throw "bad nodetype";
		for (child in _children)
			if (child.nodeType == Xml.Element)
				return child;
		return null;
	}

	public function addChild( x : Xml ) : Void {
		if( _children == null ) throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.push( x );
	}

	public function removeChild( x : Xml ) : Bool {
		if( _children == null ) throw "bad nodetype";
		var b = _children.remove( x );
		if( b )
			x._parent = null;
		return b;
	}

	public function insertChild( x : Xml, pos : Int ) : Void {
		if( _children == null ) throw "bad nodetype";
		if( x._parent != null ) x._parent._children.remove(x);
		x._parent = this;
		_children.insert( pos, x );
	}

	public function toString() : String {
		if( nodeType == Xml.PCData )
			return _fromCustomParser ? StringTools.htmlEscape(_nodeValue) : _nodeValue;

		var s = "";

		if( nodeType == Xml.Element ) {
			s += "<";
			s += _nodeName;
			for( k in _attributes.keys() ){
				s += " ";
				s += k;
				s += "=\""; // \"
				s += _attributes.get(k);
				s += "\""; // \"
			}
			if( _children.length == 0 ) {
				s += "/>";
				return s;
			}
			s += ">";
		} else if( nodeType == Xml.CData )
			return "<![CDATA["+_nodeValue+"]]>";
		else if( nodeType == Xml.Comment )
			return "<!--"+_nodeValue+"-->";
		else if( nodeType == Xml.DocType )
			return "<!DOCTYPE "+_nodeValue+">";
		else if ( nodeType == Xml.ProcessingInstruction )
			return "<?"+_nodeValue+"?>";


		for( x in iterator() )
			s += x.toString();

		if( nodeType == Xml.Element ) {
			s += "</";
			s += _nodeName;
			s += ">";
		}
		return s;
	}

	static function __init__() : Void untyped {
		Xml.Element = "element";
		Xml.PCData = "pcdata";
		Xml.CData = "cdata";
		Xml.Comment = "comment";
		Xml.DocType = "doctype";
		Xml.ProcessingInstruction = "processingInstruction";
		Xml.Document = "document";
	}

}
