package unit.issues;

class Issue6549 extends unit.Test {

	function test () {
		var int = 1;
		var x = try {
			throw true;
		} catch (err:Int) {
			"int";
		}
		catch (err:Dynamic) {
			"dynamic";
		}

		eq(x, "dynamic");

		var x = try {
			throw true;
		} catch (err:Int) {
			"int";
		}
		catch (err:Bool) {
			"bool";
		}
		catch (err:Dynamic) {
			"dynamic";
		}

		eq(x, "bool");
	}
}