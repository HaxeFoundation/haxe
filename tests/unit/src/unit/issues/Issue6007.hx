package unit.issues;

class Issue6007 extends Test {
	function test() {
		// test anonymous type with borrowed iterator
        var h = new haxe.ds.IntMap<Null<Int>>();
        h.set(0, -1);
        h.set(-4815, 8546);
        var k = Lambda.array({iterator : h.keys});
        eq("[0,-4815]", Std.string(k));

		// make sure instance body isn't executed twice
		var k = [];
		var f = function(){
			k.push(1);
			return {  then : function() k.push(2) }
		}
		f().then();
		eq("[1,2]", Std.string(k));
	}
}
