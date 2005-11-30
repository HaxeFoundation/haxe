extern class Node {

	static var element_node : Int = 1;
	static var text_node : Int = 3;

	var nodeName : String;
	var nodeValue : String;
	var nodeType : Int;

	// working in JS ?
	var attributes : Dynamic<String>;

	var parentNode : Node;
	var childNodes : Array<Node>;
	var firstChild : Node;
	var lastChild : Node;
	var previousSibling : Node;
	var nextSibling : Node;
	// var ownerDocument : Document; => Not in Flash

	function insertBefore( newChild : Node, refChild : Node ) : Void;

	/* renamed from removeNode in Flash */
	function removeChild( child : Node ) : Void;

	function appendChild( child : Node ) : Void;
	function hasChildNodes() : Bool;
	function cloneNode( deep : Bool ) : Node;
	function toString() : String;

	/* added in Flash */
	function replaceChild( newChild : Node, oldChild : Node ) : Void;

}