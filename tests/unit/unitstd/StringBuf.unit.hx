// add, toString
var x = new StringBuf();
x.toString() == "";
x.add(null);
x.toString() == "null";

// addChar
var x = new StringBuf();
x.addChar(32);
x.toString() == " ";

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

// identity
function identityTest(s:StringBuf) {
	return s;
}
identityTest(x) == x;