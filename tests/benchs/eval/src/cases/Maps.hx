package cases;

/** `IntMap` and `StringMap`. **/
class Maps {
	public static function run() {
		var sum = 0;
		for (k in 0...10) {
			var im = new Map<Int, Int>();
			for (i in 0...10000)
				im.set(i * 31, i);
			for (i in 0...10000)
				sum = (sum + im.get(i * 31)) & 0xFFFFFF;
			var sm = new Map<String, Int>();
			for (i in 0...5000)
				sm.set("k" + i, i);
			for (key in sm.keys())
				sum = (sum + sm.get(key) + key.length) & 0xFFFFFF;
		}
		return sum;
	}
}
