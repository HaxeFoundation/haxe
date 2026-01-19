import haxe.coro.Coroutine;

function runCoro<T>(coro:Coroutine<() -> T>) {
	$type(coro);
	final result = coro(null);
	$type(result);
	result.state;
}

function main() {
	runCoro(() -> return "foo");
}