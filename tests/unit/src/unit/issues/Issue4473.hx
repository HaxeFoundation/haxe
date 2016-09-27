package unit.issues;

class Issue4473 extends Test {
	function test1() {
		var results = [
			"home" => [
				1 => 1,
				2 => 3
			],
			"home2" => [
				1 => 3,
				2 => 2
			],
			"contact" => [
				1 => 3,
				2 => 3
			]
		];
		var scenes = ["home", "home2", "contact"];
		var indices = [1, 2];

		for (scene in scenes) {
			for (index in indices) {
				eq(results[scene][index], f1(scene, index));
			}
		}
	}

	function test2() {
		var results = [
			"home" => [
				1 => 1,
				2 => 2
			],
			"home2" => [
				1 => 3,
				2 => 3
			]
		];
		var scenes = ["home", "home2"];
		var indices = [1, 2];

		for (scene in scenes) {
			for (index in indices) {
				eq(results[scene][index], f2(scene, index));
			}
		}
	}

	static function f1(scene:String, index:Int) {
		var x = switch(scene) {
			case "home" if (index < 2): 1;
			case "home2" if (index >= 2): 2;
			case _: 3;
		}
		return x;
	}

	static function f2(scene:String, index:Int) {
		var x = switch(scene) {
			case "home" if (index < 2): 1;
			case "home" if (index >= 2): 2;
			case _: 3;
		}
		return x;
	}
}