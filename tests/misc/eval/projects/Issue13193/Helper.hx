class Helper {
	public static function wrongType():Int {
		var x:Int = "foo";
		return x;
	}

	public static function missingField():Int {
		return Helper.nope();
	}
}
