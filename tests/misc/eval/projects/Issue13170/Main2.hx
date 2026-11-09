class Main2 {
	static function f():Int {
		try {
			return 0;
		} finally {
			return 1;
		}
	}

	static function main() {
		while (true) {
			try {} finally {
				break;
			}
		}
		while (true) {
			try {} finally {
				continue;
			}
		}
	}
}
