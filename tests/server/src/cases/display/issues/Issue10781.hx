package cases.display.issues;

class Issue10781 extends DisplayTestCase {
	/**
		interface Canvas {
			var g2(get, null):Graphics;
		}

		class Graphics {
			public function begin() {}
		}

		class Main {
			static function main() {
				onRender(null);
			}

			static function onRender(canvas:Canvas) {
				final {-1-}g = canvas.g2;
				g.begin();
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(1)
		});
		runHaxeJson([], ServerMethods.Invalidate, {file: file});
		runHaxeJson([], DisplayMethods.Hover, {
			file: file,
			offset: offset(1)
		});
		var result = parseHover();
		Assert.same({
			kind: "TInst",
			args: {
				path: {
					moduleName: "Main",
					importStatus: 0,
					pack: [],
					typeName: "Graphics"
				},
				params: []
			}
		}, result.result.item.type);
	}
}
