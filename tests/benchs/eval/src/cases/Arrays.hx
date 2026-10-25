package cases;

/** Array reads, writes, pushes and iteration. **/
class Arrays {
	static function sieve(n:Int) {
		var composite = [for (i in 0...n) false];
		var count = 0;
		for (i in 2...n) {
			if (composite[i])
				continue;
			count++;
			if (i > Std.int(Math.sqrt(n)))
				continue;
			var j = i * i;
			while (j < n) {
				composite[j] = true;
				j += i;
			}
		}
		return count;
	}

	public static function run() {
		var sum = sieve(200000);
		var a = [];
		for (i in 0...100000)
			a.push((i * 7919) % 1000);
		for (k in 0...5)
			for (i in 1...a.length)
				a[i] = (a[i] + a[i - 1]) & 0xFFFF;
		for (x in a)
			sum = (sum + x) & 0xFFFFFF;
		return sum;
	}
}
