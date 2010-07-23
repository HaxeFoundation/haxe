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
		#if flash8
		// flash8 does not parse CDATA sections as PCDATA...
		eq( Xml.parse("<a><b><c/> <d/> \n <e/><![CDATA[<x>]]></b></a>").toString(), "<a><b><c/> <d/> \n <e/>&lt;x&gt;</b></a>" );
		#else
		eq( Xml.parse("<a><b><c/> <d/> \n <e/><![CDATA[<x>]]></b></a>").toString(), "<a><b><c/> <d/> \n <e/><![CDATA[<x>]]></b></a>" );
		#end
		#if (flash8 || php)
		eq( Xml.parse('"').toString(), '&quot;' ); // flash8 has bad habits of escaping entities
		#else
		eq( Xml.parse('"').toString(), '"' );
		#end
		#if flash9
		eq( Xml.parse('&quot; &lt; &gt;').toString(), '" &lt; &gt;' ); // some entities are resolved but not escaped on printing
		#else
		eq( Xml.parse('&quot; &lt; &gt;').toString(), '&quot; &lt; &gt;' );
		#end
	}

	function testComplex() {
		// this is showing some quirks with flash XML parser

		var header = '<?some header?>';
		var doctype = '<!DOCTYPE root SYSTEM "">';
		var comment = '<!--Comment-->';
		var xml = '<html><body><![CDATA[<a href="CDATA"/>&lt;]]></body></html>';

		#if flash8
		// flash8 can't parse header
		header = '';
		#end

		var x = Xml.parse(header + doctype + comment + xml);

		#if flash8
		// cdata is parsed as pcdata in flash8
		xml = '<html><body>&lt;a href=&quot;CDATA&quot;/&gt;&amp;lt;</body></html>';
		#end

		#if (flash || php)
		// doctype is well parsed but is not present in the parsed Xml (php, f8 and f9)
		doctype = '';
		#end

		#if flash8
		// comments are well parsed but are not present in the parsed Xml
		comment = '';
		#end

		eq( x.toString(), header + doctype + comment + xml);
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
		#if (flash || php)
		eq( b.firstChild(), null);
		eq( x.toString().split("\n").join("\\n"), '<a> </a><b/> \\n <c/>' );
		#else
		eq( b.firstChild().nodeValue, "");
		eq( x.toString().split("\n").join("\\n"), '<a> </a><b></b> \\n <c/>' );
		#end
		var c = el.next();
		eq( c.firstChild(), null);
	}

	function testCreate() {
		eq( Xml.createDocument().toString(), "");
		eq( Xml.createPCData("Hello").toString(), "Hello" );
		#if flash8

		// too hard for him

		#elseif flash9
		// flash9 printer love to include additional whitespaces
		eq( Xml.createCData("<x>").toString(), "<![CDATA[ <x> ]]>" );
		eq( Xml.createComment("Hello").toString(), "<!-- Hello -->" );
		eq( Xml.createProlog("XHTML").toString(), "<?XHTML ?>");
		// doctype is parsed but not printed
		eq( Xml.createDocType("XHTML").toString(), "" );
		#else
		eq( Xml.createCData("<x>").toString(), "<![CDATA[<x>]]>" );
		eq( Xml.createComment("Hello").toString(), "<!--Hello-->" );
		eq( Xml.createDocType("XHTML").toString(), "<!DOCTYPE XHTML>" );
		eq( Xml.createProlog("XHTML").toString(), "<?XHTML?>");
		#end
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

		eq( Lambda.count({ iterator : callback(x.elementsNamed,"em") }), 1 );

		h.nodeName = "xhtml:em";

		eq( Lambda.count({ iterator : callback(x.elementsNamed,"xhtml:em") }), 1 );
		eq( Lambda.count({ iterator : callback(x.elementsNamed,"em") }), 0 );

		eq( h.nodeName, "xhtml:em" );
	}

}