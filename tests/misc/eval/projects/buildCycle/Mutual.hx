class Mutual {
	static function main() {
		// the build macros of A and B read each other's fields
		trace(A.count() + B.count());
	}
}
