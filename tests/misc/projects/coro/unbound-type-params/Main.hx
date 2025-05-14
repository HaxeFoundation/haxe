import haxe.coro.Coroutine;

class C<TC> {
	public function new() {}

	public function test<TF>() {
		@:coroutine function f<TL>():{tc:TC, tf:TF, tl:TL} {
			return null;
		}

		Coroutine.run(f);
	}

	@:coroutine public function coro<TF>():{tc: TC, tf:TF} {
		return null;
	}
}

function main() {
	var c = new C();
	c.test();

	Coroutine.run(c.coro);
}