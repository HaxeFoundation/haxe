package unit.issues;
import haxe.Int64;
using haxe.Int64;

class Issue3203 extends Test {
	function test() {
		var i64 = I64(Int64.make(1,2));
		switch (i64) {
			case I64(x):
				eq(x.high,1);
				eq(x.low,2);
		}
	}
}

private enum Int64Test
{
	I64(i:Int64);
}
