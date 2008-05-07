package flash.utils;

extern class QName {
	var localName(default,null) : String;
	var uri(default,null) : String;
	// p1 can be Namespace or QName (without localName)
	public function new( p1 : Dynamic, ?localName : QName ) : Void;
}