class Stats {

	static var calls1 = [];
	static var calls2 = [];

	public static function add1 (v:Int) {
		calls1.push(v);
	}

	public static function add2 (v:Int) {
		calls2.push(v);
	}


	macro public static function check () {
		if (calls1.length != calls2.length) return macro false;
		for (i in calls1) {
			var a = calls1[i];
			var b = calls2[i];
			if (a != b) return macro false;
		}
		return macro true;
	}
}