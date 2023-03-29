package issues;

class Issue7765 {
	@:pure(false) function add(m:Issue7765)
		return this;

	function new(i:Int) {}

	static inline function pipe(m:Issue7765)
		return m;

	@:js('
		new issues_Issue7765(0).add(new issues_Issue7765(1)).add(new issues_Issue7765(2));
	')
	static function test() {
		new Issue7765(0).add(pipe(new Issue7765(1))).add(pipe(new Issue7765(2)));
	}

	@:js('
		var tmp = new issues_Issue7765(0);
		var m = new issues_Issue7765(1);
		var tmp1 = tmp.add(m);
		var m = new issues_Issue7765(2);
		tmp1.add(m);
	')
	@:analyzer(no_fusion)
	static function testNoFusion() {
		new Issue7765(0).add(pipe(new Issue7765(1))).add(pipe(new Issue7765(2)));
	}

	@:js('
		new issues_Issue7765(0).add(new issues_Issue7765(1));
	')
	static function test2() {
		new Issue7765(0).add({var m = new Issue7765(1); m; });
	}

	@:js('
		var tmp = new issues_Issue7765(0);
		var m = new issues_Issue7765(1);
		tmp.add(m);
	')
	@:analyzer(no_fusion)
	static function test2NoFusion() {
		new Issue7765(0).add({var m = new Issue7765(1); m; });
	}
}
