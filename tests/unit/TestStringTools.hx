package unit;

class TestStringTools extends Test
{

	function testHtmlEscape() {
		eq( StringTools.htmlEscape("<>&\"'"), "&lt;&gt;&amp;\"'");
		eq( StringTools.htmlEscape("<>&\"'", true), "&lt;&gt;&amp;&quot;&#039;" );
		eq( StringTools.htmlUnescape("&lt;&gt;&amp;&quot;&#039;"), "<>&\"'" );
	}
	
	function testHex() {
		eq( StringTools.hex(0xABCDEF,7), "0ABCDEF" );
		eq( StringTools.hex(-1,8), "FFFFFFFF" );
		eq( StringTools.hex(-481400000,8), "E34E6B40" );
	}
	
	function testCCA() {
		var str = "abc";
		eq( StringTools.fastCodeAt(str, 0), "a".code );
		eq( StringTools.fastCodeAt(str, 1), "b".code );
		eq( StringTools.fastCodeAt(str, 2), "c".code );
		eq(StringTools.fastCodeAt(String.fromCharCode(128), 0), 128);
		eq(StringTools.fastCodeAt(String.fromCharCode(255), 0), 255);
		f( StringTools.isEof(StringTools.fastCodeAt(str, 2)) );
		t( StringTools.isEof(StringTools.fastCodeAt(str, 3)) );
		
		t( StringTools.isEof(StringTools.fastCodeAt("", 0)) );
	}
		
	
}