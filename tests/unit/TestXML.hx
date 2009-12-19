package unit;

class TestXML extends Test {

	function textXML() {
		var x = Xml.parse('<a href="hello">World<b/></a>');
		eq( x.nodeName, "a" );
		eq( x.get("href"), "hello" );
		eq( x.get("other"), null );
		eq( x.firstChild().nodeValue, "World" );

		exc( function() Xml.parse("<node>") );
	}

}