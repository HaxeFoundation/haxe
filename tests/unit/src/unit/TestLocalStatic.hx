package unit;

class TestLocalStatic extends Test {
	function basic() {
		static var x = 1;
		static final y = "final";
		x++;
		return {x: x, y: y};
	}

	function testBasic() {
		var obj = basic();
		eq(2, obj.x);
		eq("final", obj.y);

		obj = basic();
		eq(3, obj.x);
		eq("final", obj.y);
	}
}
