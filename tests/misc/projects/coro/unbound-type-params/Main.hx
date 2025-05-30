import hxcoro.CoroRun;

class C<TC> {
	public function new() {}

	public function test<TF>() {
		@:coroutine function f<TL>():{tc:TC, tf:TF, tl:TL} {
			return null;
		}

		CoroRun.run(f);
	}

	@:coroutine public function coro<TF>():{tc: TC, tf:TF} {
		return null;
	}
}

function main() {
	var c = new C();
	c.test();

	CoroRun.run(c.coro);
}