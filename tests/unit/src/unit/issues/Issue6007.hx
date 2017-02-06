package unit.issues;

class Issue6007 extends Test {
	function test() {
		// test anonymous type with borrowed iterator
        var a = [0,4];
        var k = Lambda.array({iterator : a.iterator});
        eq("[0,4]", Std.string(k));

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
