// htmlEscape
var str = "<foo> & <bar> = 'invalid\"'";
var strEsc = "&lt;foo&gt; &amp; &lt;bar&gt; = 'invalid\"'";
var strEscQuotes = "&lt;foo&gt; &amp; &lt;bar&gt; = &#039;invalid&quot;&#039;";
StringTools.htmlEscape(str, false) == strEsc;
StringTools.htmlEscape(str, true) == strEscQuotes;

// htmlUnescape
StringTools.htmlUnescape(strEsc) == str;
StringTools.htmlUnescape(strEscQuotes) == str;

// startsWith
StringTools.startsWith("foo", "f") == true;
StringTools.startsWith("foo", "fo") == true;
StringTools.startsWith("foo", "foo") == true;
StringTools.startsWith("foo", "fooo") == false;
StringTools.startsWith("foo", "") == true;
StringTools.startsWith("", "") == true;

// endsWith
StringTools.endsWith("foo", "o") == true;
StringTools.endsWith("foo", "oo") == true;
StringTools.endsWith("foo", "foo") == true;
StringTools.endsWith("foo", "fooo") == false;
StringTools.endsWith("foo", "") == true;
StringTools.endsWith("", "") == true;

// isSpace
StringTools.isSpace("", 0) == false;
StringTools.isSpace("", 1) == false;
StringTools.isSpace(" ", -1) == false;
StringTools.isSpace("a", 0) == false;
StringTools.isSpace("  ", 0) == true;
StringTools.isSpace(" ", 0) == true;
StringTools.isSpace(" a", 0) == true;
StringTools.isSpace(String.fromCharCode(9), 0) == true;
StringTools.isSpace(String.fromCharCode(10), 0) == true;
StringTools.isSpace(String.fromCharCode(11), 0) == true;
StringTools.isSpace(String.fromCharCode(12), 0) == true;
StringTools.isSpace(String.fromCharCode(13), 0) == true;

// ltrim
StringTools.ltrim("a") == "a";
StringTools.ltrim("  a") == "a";
StringTools.ltrim("  a b") == "a b";
StringTools.ltrim("    ") == "";
StringTools.ltrim("") == "";

// rtrim
StringTools.rtrim("a") == "a";
StringTools.rtrim("a  ") == "a";
StringTools.rtrim("a b  ") == "a b";
StringTools.rtrim("    ") == "";
StringTools.rtrim("") == "";

// trim
StringTools.trim("a") == "a";
StringTools.trim("a  ") == "a";
StringTools.trim("a b  ") == "a b";
StringTools.trim("    ") == "";
StringTools.trim("") == "";
StringTools.trim("  a") == "a";
StringTools.trim("  a b") == "a b";
StringTools.trim("  a b  ") == "a b";

// lpad
StringTools.lpad("", "", 2) == "";
StringTools.lpad("", "a", 0) == "";
StringTools.lpad("b", "a", 0) == "b";
StringTools.lpad("b", "", 2) == "b";
StringTools.lpad("", "a", 2) == "aa";
StringTools.lpad("b", "a", 0) == "b";
StringTools.lpad("b", "a", 1) == "b";
StringTools.lpad("b", "a", 2) == "ab";
StringTools.lpad("b", "a", 3) == "aab";
StringTools.lpad("b", "a", 4) == "aaab";
StringTools.lpad("b", "abcdef", 4) == "abcdefb";

// rpad
StringTools.rpad("", "", 2) == "";
StringTools.rpad("", "a", 0) == "";
StringTools.rpad("b", "a", 0) == "b";
StringTools.rpad("b", "", 2) == "b";
StringTools.rpad("", "a", 2) == "aa";
StringTools.rpad("b", "a", 0) == "b";
StringTools.rpad("b", "a", 1) == "b";
StringTools.rpad("b", "a", 2) == "ba";
StringTools.rpad("b", "a", 3) == "baa";
StringTools.rpad("b", "a", 4) == "baaa";
StringTools.rpad("b", "abcdef", 4) == "babcdef";

// replace
var s = "xfooxfooxxbarxbarxx";
StringTools.replace(s, "x", "") == "foofoobarbar";
StringTools.replace(s, "", "") == "xfooxfooxxbarxbarxx";
StringTools.replace(s, "", "x") == "xxfxoxoxxxfxoxoxxxxxbxaxrxxxbxaxrxxxx";

// hex
StringTools.hex(0, 0) == "0";
StringTools.hex(0, 1) == "0";
StringTools.hex(0, 2) == "00";
StringTools.hex(1, 2) == "01";
StringTools.hex(4564562) == "45A652";
StringTools.hex(4564562, 0) == "45A652";
StringTools.hex(4564562, 1) == "45A652";
StringTools.hex( -1) == "FFFFFFFF";
StringTools.hex( -2) == "FFFFFFFE";
StringTools.hex(0xABCDEF, 7) == "0ABCDEF";
StringTools.hex( -1, 8) == "FFFFFFFF";
StringTools.hex( -481400000, 8) == "E34E6B40";

// fastCodeAt
var s = "foo1bar";
StringTools.fastCodeAt(s, 0) == 102;
StringTools.fastCodeAt(s, 1) == 111;
StringTools.fastCodeAt(s, 2) == 111;
StringTools.fastCodeAt(s, 3) == 49;
StringTools.fastCodeAt(s, 4) == 98;
StringTools.fastCodeAt(s, 5) == 97;
StringTools.fastCodeAt(s, 6) == 114;
var str = "abc";
StringTools.fastCodeAt(str, 0) == "a".code;
StringTools.fastCodeAt(str, 1) == "b".code;
StringTools.fastCodeAt(str, 2) == "c".code;
StringTools.fastCodeAt(String.fromCharCode(128), 0) == 128;
StringTools.fastCodeAt(String.fromCharCode(255), 0) == 255;
StringTools.isEof(StringTools.fastCodeAt(str, 2)) == false;
StringTools.isEof(StringTools.fastCodeAt(str, 3)) == true;
StringTools.isEof(StringTools.fastCodeAt("", 0)) == true;
		
// isEOF
#if neko
StringTools.isEof(null) == true;
#elseif (cs || java)
StringTools.isEof( -1) == true;
#elseif js
// how do I test this here?
#else
StringTools.isEof(0) == true;
#end
