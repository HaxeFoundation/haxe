package unit.issues;
import unit.Test;

class Issue9597 extends Test {
	function test() {
		var data = [
			{name: 'Page A', uv: 4000, pv: 2400, amt: 2400},
			{name: 'Page B', uv: 3000, pv: 1398, amt: 2210},
			{name: 'Page C', uv: -1000, pv: 9800, amt: 2290},
			{name: 'Page D', uv: 500, pv: 3908, amt: 2000},
			{name: 'Page E', uv: -2000, pv: 4800, amt: 2181},
			{name: 'Page F', uv: -250, pv: 3800, amt: 2500},
			{name: 'Page G', uv: 3490, pv: 4300, amt: 2100}
		  ];

		  foo(data);
		  utest.Assert.pass();
	}

	static function foo<T:{uv:Int}>(data:Array<T>):Void {}
}
