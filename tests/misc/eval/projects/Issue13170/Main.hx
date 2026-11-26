class Main {
	static function main() {
		var log = [];
		for (i in 0...3) {
			try {
				if (i == 1)
					continue;
				log.push('try $i');
			} finally {
				// loops, break and continue inside `finally` are fine
				for (j in 0...3) {
					if (j == 1)
						continue;
					if (j == 2)
						break;
					log.push('finally $i');
				}
				// so are local functions with returns
				var f = () -> {
					return i;
				}
				f();
			}
		}
		// `finally` is still a valid identifier
		var finally = log.join(", ");
		Sys.println(finally);
	}
}
