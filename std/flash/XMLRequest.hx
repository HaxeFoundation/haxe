package flash;

class XMLRequest {

	public static function send( url : String, xml : Xml, ?target : String, ?post : Bool, ?content : String ) {
		var x : Dynamic = untyped xml.__x;
		x.contentType = if( content == null ) "text/xml" else content;
		x.send(url,if( target == null ) "_self" else target,if( post ) "POST" else "GET");
	}

	public static function load( url : String, xml : Xml, onData : Null<String> -> Void, ?post : Bool, ?content : String ) {
		var x : Dynamic = untyped xml.__x;
		var recv = untyped __new__(_global["XML"]);
		recv.onData = onData;
		x.contentType = if( content == null ) "text/xml" else content;
		x.sendAndLoad(url,recv);
	}

}