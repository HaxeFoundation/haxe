package unit.issues;

class Issue6315 extends unit.Test {
	function test() {
		try if(throw "ok") { } catch(e:Dynamic) { }
		try if(throw "ok") { } else { } catch(e:Dynamic) { }
		try switch throw "ok" { case _: } catch(e:Dynamic) { }

		try if(return "ok") { } catch(e:Dynamic) { }
		try if(return "ok") { } else { } catch(e:Dynamic) { }
		try switch return "ok" { case _: } catch(e:Dynamic) { }

		while (true) {
			try if(break) { } catch(e:Dynamic) { }
			try if(break) { } else { } catch(e:Dynamic) { }
			try switch break { case _: } catch(e:Dynamic) { }

			try if(continue) { } catch(e:Dynamic) { }
			try if(continue) { } else { } catch(e:Dynamic) { }
			try switch continue { case _: } catch(e:Dynamic) { }
		}

		return "";
	}
}
