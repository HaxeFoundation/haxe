package cases;

/** Anonymous structures and `Dynamic` values. **/
class Anon {
	public static function run() {
		var points:Array<{x:Int, y:Int, ?tag:String}> = [];
		for (i in 0...1000)
			points.push({x: i, y: i * 2});
		var sum = 0;
		for (k in 0...100) {
			for (p in points) {
				sum = (sum + p.x + p.y) & 0xFFFFFF;
				p.x = (p.x + 1) & 0xFFFF;
			}
		}
		var dyns:Array<Dynamic> = [for (i in 0...1000) {a: i, b: {c: i + 1}}];
		for (k in 0...50)
			for (d in dyns)
				sum = (sum + d.a + d.b.c) & 0xFFFFFF;
		return sum;
	}
}
