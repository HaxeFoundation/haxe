function baz() {
	var foo = function(i:Int, i:Int) {};
	function bar(i:Int, i:Int) {}
	function bar1(i:Int, j:Int, i:Int) {}
	function bar2(i:Int, ?i:Int) {}
	function bar3(i:Int, i = 42) {}
	var lambda = (i, i) -> {};
}
