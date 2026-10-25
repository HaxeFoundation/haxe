package cases;

/** String building, concatenation and scanning. **/
class Strings {
	public static function run() {
		var sum = 0;
		for (k in 0...20) {
			var b = new StringBuf();
			for (i in 0...2000) {
				b.add("item");
				b.add(i);
				b.addChar(",".code);
			}
			var s = b.toString();
			for (i in 0...s.length)
				sum = (sum + StringTools.fastCodeAt(s, i)) & 0xFFFFFF;
			var parts = s.split(",");
			var t = "";
			for (p in parts)
				if (p.length > 6)
					t += p.substr(4);
			sum = (sum + t.length + parts.length) & 0xFFFFFF;
		}
		return sum;
	}
}
