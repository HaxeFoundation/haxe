package issues;

class Issue9388 {
	var x(default,set):Int;
	function set_x(v) return v;

	@:js('
		this.set_x(this.x + 1);
	')
	function f() {
		x++;
	}
}
