class Main4 {
	static function main() {
		for (i in 0...3) {
			try {
				if (i == 1)
					break;
			} finally {}
		}
		try {
			return;
		} finally {}
	}
}
