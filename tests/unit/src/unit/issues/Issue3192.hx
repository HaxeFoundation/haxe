package unit.issues;

class Issue3192 extends Test {
	function test() {
		var x1 = {x:1, y:2};
		var x2 = ({x:1, y:2}:{x:Int,y:Int});
		var x3:{x:Int,y:Int} = {x:1, y:2};
		var y1:{} = x1;
		var y2:{} = x2;
		var y3:{} = x3;
		var z1:{x:Int} = x1;
		var z2:{x:Int} = x2;
		var z3:{x:Int} = x3;
		eq(1, x1.x);
		eq(2, x1.y);
	}
}