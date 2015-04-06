package unit;

class TestXML extends Test {

	function checkExc( x : Xml, ?pos ) {
		exc( function() x.nodeName, pos );
		exc( function() x.nodeValue, pos );
		exc( function() x.attributes(), pos );
		exc( function() x.get("att"), pos );
		exc( function() x.exists("att"), pos );
	}

	function testBasic() {
		var x = Xml.parse('<a href="hello">World<b/></a>');

		t( x.firstChild() == x.firstChild() );

		eq( x.nodeType, Xml.Document );
		checkExc(x);

		x = x.firstChild();
		eq( x.nodeType, Xml.Element );


		// nodeName
		eq( x.nodeName, "a" );
		x.nodeName = "b";
		eq( x.nodeName, "b" );
		eq( x.toString(), '<b href="hello">World<b/></b>');

		// attributes
		eq( x.get("href"), "hello" );
		eq( x.get("other"), null );
		eq( x.exists("href"), true );
		eq( x.exists("other"), false );
		eq( Lambda.array({ iterator : x.attributes }).join("#"), "href" );
		x.remove("href");
		eq( Lambda.array({ iterator : x.attributes }).join("#"), "" );
		eq( x.toString(), '<b>World<b/></b>');

		// children
		eq( x.firstChild().nodeValue, "World" );
		eq( x.firstElement().nodeName, "b" );

		// errors
		exc( function() Xml.parse("<node>") );
	}

	function testFormat() {
		eq( Xml.parse("<a><b><c/> <d/> \n <e/><![CDATA[<x>]]></b></a>").toString(), "<a><b><c/> <d/> \n <e/><![CDATA[<x>]]></b></a>" );
		eq( Xml.parse('"').toString(), '"' );
		//eq( Xml.parse('&quot; &lt; &gt;').toString(), '&quot; &lt; &gt;' );
	}

	function testComplex() {
		// this is showing some quirks with flash XML parser

		var header = '<?some header?>';
		var doctype = '<!DOCTYPE root SYSTEM "">';
		var comment = '<!--Comment-->';
		var xml = '<html><body><![CDATA[<a href="CDATA"/>&lt;]]></body></html>';

		var x = Xml.parse(header + doctype + comment + xml);
		eq( x.toString(), header + doctype + comment + xml);

		return; // too hard for him
	}

	function testWhitespaces() {
		// whitespaces
		var x = Xml.parse('<a> </a><b></b> \n <c/>');

		var childs = Lambda.array(x);

		eq( childs.length, 4 );

		var d = childs[2];
		eq( d.nodeType, Xml.PCData );
		eq( d.nodeValue, " \n " );

		var el = x.elements();
		var a = el.next();
		eq( a.firstChild().nodeValue, " ");
		var b = el.next();
		eq( b.firstChild().nodeValue, "");
		eq( x.toString().split("\n").join("\\n"), '<a> </a><b></b> \\n <c/>' );
		var c = el.next();
		eq( c.firstChild(), null);
	}

	function testCreate() {
		eq( Xml.createDocument().toString(), "");
		eq( Xml.createPCData("Hello").toString(), "Hello" );

		eq( Xml.createCData("<x>").toString(), "<![CDATA[<x>]]>" );
		eq( Xml.createComment("Hello").toString(), "<!--Hello-->" );

		eq( Xml.createProcessingInstruction("XHTML").toString(), "<?XHTML?>");
		eq( Xml.createDocType("XHTML").toString(), "<!DOCTYPE XHTML>" );

		eq( Xml.parse("<!--Hello-->").firstChild().nodeValue, "Hello" );
		var c = Xml.createComment("Hello");
		eq( c.nodeValue, "Hello" );
		c.nodeValue = "Blabla";
		eq( c.nodeValue, "Blabla" );
		eq( c.toString(), "<!--Blabla-->");
		eq( Xml.parse("<![CDATA[Hello]]>").firstChild().nodeValue, "Hello" );
		var c = Xml.createCData("Hello");
		eq( c.nodeValue, "Hello" );
		c.nodeValue = "Blabla";
		eq( c.nodeValue, "Blabla" );
		eq( c.toString(), "<![CDATA[Blabla]]>");
		eq( Xml.createPCData("Hello").nodeValue, "Hello" );

		return;
	}

	function testNS() {
		var x = Xml.parse('<xhtml:br xmlns:xhtml="http://www.w3.org/1999/xhtml" xhtml:alt="test"><hello/></xhtml:br>').firstChild();
		eq( x.nodeType, Xml.Element );
		eq( x.nodeName, "xhtml:br" );
		t( x.exists("xhtml:alt") );
		eq( x.get("xhtml:alt"), "test" );
		eq( x.get("xhtml:other"), null );
		x.set("xhtml:alt", "bye" );
		eq( x.get("xhtml:alt"), "bye" );

		var h = x.firstElement();
		eq( h.nodeName, "hello" );
		h.nodeName = "em";
		eq( h.nodeName, "em" );

		eq( Lambda.count({ iterator : x.elementsNamed.bind("em") }), 1 );

		h.nodeName = "xhtml:em";

		eq( Lambda.count({ iterator : x.elementsNamed.bind("xhtml:em") }), 1 );
		eq( Lambda.count({ iterator : x.elementsNamed.bind("em") }), 0 );

		eq( h.nodeName, "xhtml:em" );
	}

	function testNodetype() {
		var element = Xml.createElement("x");

		var l = [Xml.createPCData("x"), Xml.createCData("x"), Xml.createDocType("x"), Xml.createProcessingInstruction("x")];
		for (xml in l)
		{
			exc(function() xml.firstChild());
			exc(function() xml.firstElement());
			exc(function() xml.elements());
			exc(function() xml.elementsNamed("x"));
			exc(function() xml.addChild(element));
			exc(function() xml.removeChild(element));
			exc(function() xml.insertChild(element, 0));
			exc(function() for (x in xml) null);
		}
	}

	function testCustomXmlParser() {
		var entities = ["&lt;", "&gt;", "&quot;", "&amp;", "&apos;", "&euro;", "&#64;", "&#244;", "&#x3F;", "&#xFF;"];
		var values = ['<', '>', '"', '&', "'", '&euro;', '@', "ô", String.fromCharCode(0x3F), "ÿ"];

		for( i in 0...entities.length) {
			infos(entities[i]);
			eq( haxe.xml.Parser.parse(entities[i], false).firstChild().nodeValue, values[i] );
		}

		var s = "<a>&gt;<b>&lt;</b>&lt;&gt;<b>&gt;&lt;</b>\"</a>";
		var xml = haxe.xml.Parser.parse(s);
		eq(s, xml.toString());
	}

	function testMore() {
		var doc = Xml.parse("<a>A</a><i>I</i>");
		var aElement = doc.elementsNamed('a').next();
		var iElement = doc.elementsNamed('i').next();
		iElement.addChild(aElement);

		eq(doc.toString(), "<i>I<a>A</a></i>");
	}

	function testIssue2299() {
		var xml = Xml.parse("<xml>Hä?</xml>");
		eq('<xml>Hä?</xml>', xml.firstElement().toString());
		eq('Hä?', xml.firstElement().firstChild().nodeValue);
	}

	function testIssue2739() {
		var simpleContent = "My &amp; &lt;You&gt;";
		var node1 = Xml.parse(simpleContent).firstChild();
		eq( node1.toString(), simpleContent );

		// TODO?
		//var content2 = "&laquo;&#64;&raquo;";
		//var node3 = Xml.parse(content2).firstChild();
		//eq( node3.toString(), content2 );
	}

	function testIssue3058() {
        var xml = Xml.createElement("node");
        xml.set("key", 'a"b\'&c>d<e');
        eq('a"b\'&c>d<e', xml.get("key"));
        eq('<node key="a&quot;b&#039;&amp;c&gt;d&lt;e"/>', xml.toString());
	}

	function testIssue3630() {
		exc(function() haxe.xml.Parser.parse("<node attribute='<'/>", true));
		exc(function() haxe.xml.Parser.parse("<node attribute='>'/>", true));

		var a = Xml.parse('<node attribute="something with &lt; &amp; &quot; &apos; special characters &gt;"/>');
		var c = a.firstChild();
		eq('something with < & " \' special characters >', c.get("attribute"));

		var a = Xml.parse('<div e="event=Hit.Eject&#x0D;&#x0A;&quot;onHit"></div>');
		var c = a.firstChild();
		eq('event=Hit.Eject\r\n"onHit', c.get("e"));
	}

	function testIssue4139() {
		function doXml(data:String)
		{
			var xml = Xml.parse(data);
			var first = xml.firstElement();
			var thing = first.firstChild();
			return "blah = " + thing.get("blah");
		}
		var fancyData = '<?xml version="1.0" encoding="utf-8" ?><data><thing blah="abc&def"/></data>';
		eq("blah = abc&def", doXml(fancyData));
		var plainData = '<data><thing blah="abc&def"/></data>';
		eq("blah = abc&def", doXml(plainData));
	}
}
