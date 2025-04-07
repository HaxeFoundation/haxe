class Main {
	static function main() {
		var s1 : Single = 10.0;
		var s2 : Single = 0.3;
		var f1 : Float = 10.0;
		var f2 : Float = 0.3;
		$type(s1 + s2);
		$type(s1 - s2);
		$type(s1 * s2);
		$type(s1 / s2);
		$type(s1 / (f2 : Single));
		$type((f1 : Single) / s2);
		$type((f1 : Single) / (f2 : Single));
		$type(f1 / s2);
		$type(s1 / f2);
	}
}
