// add, toString
var x = new StringBuf();
x.toString() == "";
x.length == 0;
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

// surrogate characters
#if !(neko)
var x = new StringBuf();
x.add("👽");
x.toString() == "👽";
var x = new StringBuf();
x.addChar(0x1F47D);
x.toString() == "👽";
var x = new StringBuf();
#if utf16
x.addSub("a👽b", 1, 2);
#else
x.addSub("a👽b", 1, 1);
#end
x.toString() == "👽";
#end

// StringBuf can store multiple elements
final x = new StringBuf();
x.add("ab");
x.add("cd");
x.addChar("e".code);
x.add("fg");
x.toString() == "abcdefg";

// Calling toString() does not empty the buffer
x.toString() == "abcdefg";
x.toString() == "abcdefg";
x.length == 7;

// identity
function identityTest(s:StringBuf) {
	return s;
}
identityTest(x) == x;

// Clearing a buffer resets its visible state
x.length > 0;
x.clear();
x.toString() == "";
x.length == 0;

// Previously cleared buffers do not leak past state
x.add("foo");
x.toString() == "foo";
x.length == 3;

// Buffers can be cleared multiple times
x.clear();
x.length == 0;
x.clear();
x.clear();
x.clear();
x.length == 0;

// Buffers can be cleared immediately after creation
// (ie. `clear` does not depend on any private state being non-null)
final x = new StringBuf();
x.clear();
x.toString() == "";
x.length == 0;
