package unit.issues;

/**
 * Tests if String's indexOf/lastIndexOf functions behave ECMAScript compliant
 */
class Issue5271 extends unit.Test {
	#if !flash
	function test() {
		/*
		 * test indexOf
		 */
		eq(0, "".indexOf(""));
		eq(0, "".indexOf("", 0));
		eq(0, "".indexOf("", 1));
		eq(0, "".indexOf("", -1));

		eq(0, " ".indexOf(""));
		eq(0, " ".indexOf("", 0));
		eq(1, " ".indexOf("", 1));
		eq(1, " ".indexOf("", 2));
		eq(0, " ".indexOf("", -1));

		eq(0, "dog".indexOf(""));
		eq(0, "dog".indexOf("", 0));
		eq(1, "dog".indexOf("", 1));
		eq(2, "dog".indexOf("", 2));
		eq(3, "dog".indexOf("", 3));
		eq(3, "dog".indexOf("", 4));
		eq(3, "dog".indexOf("", 10));
		#if !lua
		eq(0, "dog".indexOf("", -1));
		#end

		eq(-1, "dogdog".indexOf("cat"));
		eq(3, "dogcat".indexOf("cat"));
		eq(3, "dogcat".indexOf("cat", 0));
		eq(3, "dogcat".indexOf("cat", 1));
		eq(3, "catcat".indexOf("cat", 3));
		eq(-1, "catcat".indexOf("cat", 4));

		/*
		 * test lastIndexOf
		 */
		eq(0, "".lastIndexOf(""));
		eq(0, "".lastIndexOf("", 0));
		eq(0, "".lastIndexOf("", 1));
		eq(0, "".lastIndexOf("", -1));

		eq(1, " ".lastIndexOf(""));
		eq(0, " ".lastIndexOf("", 0));
		eq(1, " ".lastIndexOf("", 1));
		eq(1, " ".lastIndexOf("", 2));
		eq(0, " ".lastIndexOf("", -1));

		eq(3, "dog".lastIndexOf(""));
		eq(0, "dog".lastIndexOf("", 0));
		eq(1, "dog".lastIndexOf("", 1));
		eq(2, "dog".lastIndexOf("", 2));
		eq(3, "dog".lastIndexOf("", 3));
		eq(3, "dog".lastIndexOf("", 4));
		eq(3, "dog".lastIndexOf("", 10));
		eq(0, "dog".lastIndexOf("", -1));

		eq(-1, "dogdog".lastIndexOf("cat"));
		eq(3, "dogcat".lastIndexOf("cat"));
		eq(-1, "dogcat".lastIndexOf("cat", 0));
		eq(-1, "dogcat".lastIndexOf("cat", 1));
		eq(3, "catcat".lastIndexOf("cat", 3));
		eq(3, "catcat".lastIndexOf("cat", 4));
	}
	#end
}
