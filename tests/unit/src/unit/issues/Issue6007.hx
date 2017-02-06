package unit.issues;

class Issue6007 extends Test {
	function test() {
#if !flash
		// test anonymous type with borrowed iterator
		var a = [0,4];
		var k = Lambda.array({iterator : a.iterator});
		eq("[0,4]", Std.string(k));
#end

		// make sure instance body isn't executed twice
		var k = [];
		var f = function(){
			k.push(1);
			return {  then : function(x) k.push(x) }
		}
		f().then(2);
		eq("[1,2]", Std.string(k));
	}
}
