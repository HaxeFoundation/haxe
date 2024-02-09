package unit.issues;

class Issue9905 extends unit.Test {
	function test() {
		var v = Std.random(10);
		var v1 = -1;
		var v2 = switch v {
			case (
				_ =>
				({
					while(Std.random(1) > 0) {
						v1 = _;
					}
					v1 = _;
				})
				=> v3
			):
				v3;
		}
		eq(v, v1);
		eq(v, v2);

		var v = Std.random(10);
		var v1 = -1;
		var v2 = switch v {
			case (
				_ =>
				({
					try {
						v1 = _;
					} catch(e) {
						_;
					}
				})
				=> v3
			):
				v3;
		}
		eq(v, v1);
		eq(v, v2);
	}
}