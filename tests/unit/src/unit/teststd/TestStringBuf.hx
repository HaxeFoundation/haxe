package unit.teststd;

class TestStringBuf extends unit.Test {
	public function test() {
		// add, toString
		var x = new StringBuf();
		eq(x.toString(), "");
		eq(x.length, 0);
		x.add(null);
		eq(x.toString(), "null");

		// addChar
		var x = new StringBuf();
		x.addChar(32);
		eq(x.toString(), " ");

		// addSub
		var x = new StringBuf();
		x.addSub("abcdefg", 1);
		eq(x.toString(), "bcdefg");
		var x = new StringBuf();
		x.addSub("abcdefg", 1, null);
		eq(x.toString(), "bcdefg");
		var x = new StringBuf();
		x.addSub("abcdefg", 1, 3);
		eq(x.toString(), "bcd");

		// surrogate characters
		#if !(neko)
		var x = new StringBuf();
		x.add("👽");
		eq(x.toString(), "👽");
		var x = new StringBuf();
		x.addChar(0x1F47D);
		eq(x.toString(), "👽");
		var x = new StringBuf();
		#if utf16
		x.addSub("a👽b", 1, 2);
		#else
		x.addSub("a👽b", 1, 1);
		#end
		eq(x.toString(), "👽");
		#end

		// StringBuf can store multiple elements
		final x = new StringBuf();
		x.add("ab");
		x.add("cd");
		x.addChar("e".code);
		x.add("fg");
		eq(x.toString(), "abcdefg");

		// Calling toString() does not empty the buffer
		eq(x.toString(), "abcdefg");
		eq(x.toString(), "abcdefg");
		eq(x.length, 7);

		// identity
		function identityTest(s:StringBuf) {
			return s;
		}
		eq(identityTest(x), x);

		// Clearing a buffer resets its visible state
		t(x.length > 0);
		x.clear();
		eq(x.toString(), "");
		eq(x.length, 0);

		// Previously cleared buffers do not leak past state
		x.add("foo");
		eq(x.toString(), "foo");
		eq(x.length, 3);

		// Buffers can be cleared multiple times
		x.clear();
		eq(x.length, 0);
		x.clear();
		x.clear();
		x.clear();
		eq(x.length, 0);

		// Buffers can be cleared immediately after creation
		// (ie. `clear` does not depend on any private state being non-null)
		final x = new StringBuf();
		x.clear();
		eq(x.toString(), "");
		eq(x.length, 0);

	}
}
