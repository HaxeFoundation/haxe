package issues;

class Issue9778Class {
	@:isVar public var x(get, set):Float;
	public var y(get, default):Float;
	public var z(default, set):Float;

	function get_x()
		return x;

	function set_x(v:Float)
		return x = v;

	function get_y()
		return y;

	function set_z(v:Float) {
		return z = v;
	}

	public var next:Issue9778Class;

	public function new() {}
}

/*
	The purpose of this test is to test what kind of output the typer produces, which is why we have
	@:analyzer(ignore) here.
*/
@:analyzer(ignore)
class Issue9778 {
	@:js("
		var c = new issues_Issue9778Class();
		c.set_x(c.get_x() + 1);
		c.y = c.get_y() + 1;"
	)
	static function testUnnecessaryPostfix() {
		var c = new Issue9778Class();
		c.x++;
		c.y++;
	}

	@:js('
		var c = new issues_Issue9778Class();
		c.set_x(c.get_x() + 1);
		c.y += 1;
		c.set_z(c.z + 1);
		c.set_x(c.get_x() + 1);
		c.y = c.get_y() + 1;
		c.set_z(c.z + 1);
		c.set_x(c.get_x() + 1);
		c.y = c.get_y() + 1;
		c.set_z(c.z + 1);
	')
	static function testLhsTempvarDepth0NoValue() {
		var c = new Issue9778Class();
		c.x += 1;
		c.y += 1;
		c.z += 1;
		++c.x;
		++c.y;
		++c.z;
		c.x++;
		c.y++;
		c.z++;
	}

	@:js('
		var c = new issues_Issue9778Class();
		c.next = c;
		var fh = c.next;
		fh.set_x(fh.get_x() + 1);
		var fh = c.next;
		fh.y += 1;
		var fh = c.next;
		fh.set_z(fh.z + 1);
		var fh = c.next;
		fh.set_x(fh.get_x() + 1);
		var fh = c.next;
		fh.y = fh.get_y() + 1;
		var fh = c.next;
		fh.set_z(fh.z + 1);
		var fh = c.next;
		fh.set_x(fh.get_x() + 1);
		var fh = c.next;
		fh.y = fh.get_y() + 1;
		var fh = c.next;
		fh.set_z(fh.z + 1);
	')
	static function testLhsTempvarDepth1NoValue() {
		var c = new Issue9778Class();
		c.next = c;
		c.next.x += 1;
		c.next.y += 1;
		c.next.z += 1;
		++c.next.x;
		++c.next.y;
		++c.next.z;
		c.next.x++;
		c.next.y++;
		c.next.z++;
	}

	@:js('
		var c = new issues_Issue9778Class();
		issues_Issue9778.use(c.set_x(c.get_x() + 1));
		issues_Issue9778.use(c.y += 1);
		issues_Issue9778.use(c.set_z(c.z + 1));
		issues_Issue9778.use(c.set_x(c.get_x() + 1));
		issues_Issue9778.use(c.y = c.get_y() + 1);
		issues_Issue9778.use(c.set_z(c.z + 1));
	')
	static function testLhsTempvarDepth0Value() {
		var c = new Issue9778Class();
		use(c.x += 1);
		use(c.y += 1);
		use(c.z += 1);
		use(++c.x);
		use(++c.y);
		use(++c.z);
		/*
			Postfix in value-places generates some nasty code with @:analyzer(ignore) which I don't want to add as a test:
			issues_Issue9778.use((function($this) {var $r;var lhs = c.get_x();var postfix = lhs;c.set_x(lhs + 1);$r = postfix;return $r;}(this)));
			issues_Issue9778.use((function($this) {var $r;var lhs = c.get_y();var postfix = lhs;c.y = lhs + 1;$r = postfix;return $r;}(this)));
			issues_Issue9778.use((function($this) {var $r;var postfix = c.z;c.set_z(c.z + 1);$r = postfix;return $r;}(this)));
		*/
		// use(c.x++);
		// use(c.y++);
		// use(c.z++);
	}

	/*
		This is consistent but really ugly, so we shouldn't assert the exact output.

	@:js('
		var c = new issues_Issue9778Class();
		c.next = c;
		issues_Issue9778.use((function($this) {var $r;var fh = c.next;$r = fh.set_x(fh.get_x() + 1);return $r;}(this)));
		issues_Issue9778.use((function($this) {var $r;var fh = c.next;$r = fh.y += 1;return $r;}(this)));
		issues_Issue9778.use((function($this) {var $r;var fh = c.next;$r = fh.set_z(fh.z + 1);return $r;}(this)));
		issues_Issue9778.use((function($this) {var $r;var fh = c.next;$r = fh.set_x(fh.get_x() + 1);return $r;}(this)));
		issues_Issue9778.use((function($this) {var $r;var fh = c.next;$r = fh.y = fh.get_y() + 1;return $r;}(this)));
		issues_Issue9778.use((function($this) {var $r;var fh = c.next;$r = fh.set_z(fh.z + 1);return $r;}(this)));
		issues_Issue9778.use((function($this) {var $r;var fh = c.next;var lhs = fh.get_x();var postfix = lhs;fh.set_x(lhs + 1);$r = postfix;return $r;}(this)));
		issues_Issue9778.use((function($this) {var $r;var fh = c.next;var lhs = fh.get_y();var postfix = lhs;fh.y = lhs + 1;$r = postfix;return $r;}(this)));
		issues_Issue9778.use((function($this) {var $r;var fh = c.next;var postfix = fh.z;fh.set_z(fh.z + 1);$r = postfix;return $r;}(this)));
	')
	@:analyzer(ignore)
	static function testLhsTempvarDepth1Value() {
		var c = new Issue9778Class();
		c.next = c;
		use(c.next.x += 1);
		use(c.next.y += 1);
		use(c.next.z += 1);
		use(++c.next.x);
		use(++c.next.y);
		use(++c.next.z);
		use(c.next.x++);
		use(c.next.y++);
		use(c.next.z++);
	}
	*/

	@:pure(false)
	static public function use<T>(t:T) { return t; }
}