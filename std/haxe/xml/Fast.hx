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

	var __x : Xml;

	public var data(getData,null) : String;
	public var node(default,null) : NodeAccess;
	public var nodes(default,null) : NodeListAccess;
	public var att(default,null) : AttribAccess;

	public function new( x : Xml ) {
		if( x.nodeType != Xml.Document && x.nodeType != Xml.Element )
			throw "Invalid nodeType "+x.nodeType;
		__x = x;
		node = new NodeAccess(x);
		nodes = new NodeListAccess(x);
		att = new AttribAccess(x);
	}

	public function node() {
		return __x;
	}

	function name() {
		return if( __x.nodeType == Xml.Document ) "Document" else __x.nodeName;
	}

	function getData() {
		var it = __x.iterator();
		if( !it.hasNext() )
			throw name()+" does not have data";
		var v = it.next();
		if( it.hasNext() )
			throw name()+" does not only have data";
		if( v.nodeType != Xml.PCData && v.nodeType != Xml.CData )
			throw name()+" does not have data";
		return v.nodeValue;
	}

}
