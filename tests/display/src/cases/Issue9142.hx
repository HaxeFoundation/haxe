package cases;

class Issue9142 extends DisplayTestCase {
	/**
		import NonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentImport() {
		eq("String", type(pos(1)));
	}

	/**
		import lowercase;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testLowercaseImport() {
		eq("String", type(pos(1)));
	}

	/**
		import haxe.Int64.__Int64;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testPrivateImport() {
		eq("String", type(pos(1)));
	}

	/**
		import StringTools as st;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testLowercaseAliasImport() {
		eq("String", type(pos(1)));
	}

	/**
		import StringTools.NonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeImport() {
		eq("String", type(pos(1)));
	}

	/**
		import StringTools.StringTools.NonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeFieldImport() {
		eq("String", type(pos(1)));
	}

	/**
		import StringTools.StringTools.nonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeFieldImport2() {
		eq("String", type(pos(1)));
	}

	/**
		import StringTools.StrongTools.NonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeFieldImport3() {
		eq("String", type(pos(1)));
	}

	/**
		import StringTools.StrongTools.nonExistent;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeFieldImport4() {
		eq("String", type(pos(1)));
	}

	/**
		import StringTools.StringTools.StringTools.StringTools;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testTooMuchImport() {
		eq("String", type(pos(1)));
	}

	/**
		import StringTools.StringTools.StringTools.StringTools.*;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testTooMuchImportAll() {
		eq("String", type(pos(1)));
	}

	/**
		import StringTools.StrongTools.*;

		class Main {
			static function main() {
				"fo{-1-}o"
			}
		}
	**/
	function testNonExistentSubtypeAll() {
		eq("String", type(pos(1)));
	}
}
