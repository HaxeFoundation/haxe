// add, toString
var x = new StringBuf();
x.toString() == "";
x.add(null);
x.toString() == "null";
var x = new StringBuf();
x.add("👽");
x.toString() == "👽";

// addChar
var x = new StringBuf();
x.addChar(32);
x.toString() == " ";
var x = new StringBuf();
x.addChar(0x1F47D);
x.toString() == "👽";

// addSub
var x = new StringBuf();
x.addSub("abcdefg", 1);
x.toString() == "bcdefg";
var x = new StringBuf();
x.addSub("abcdefg", 1, null);
x.toString() == "bcdefg";
var x = new StringBuf();
x.addSub("abcdefg", 1, 3);
x.toString() == "bcd";
var x = new StringBuf();
#if utf16
x.addSub("a👽b", 1, 2);
#else
x.addSub("a👽b", 1, 1);
#end
x.toString() == "👽";

// identity
function identityTest(s:StringBuf) {
	return s;
}
identityTest(x) == x;