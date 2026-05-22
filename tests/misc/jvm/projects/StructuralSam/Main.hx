import test.Listeners;
import test.Listeners.Listeners_OnClick;
import test.Listeners.Listeners_WithToString;
import test.Listeners.Listeners_AbstractEqualsPlusOne;
import test.Listeners.Listeners_WithDefaults;
import test.Listeners.Listeners_StringMaker;
import test.Listeners.Listeners_Unused;
import test.Listeners.Listeners_CtorSam;
import test.Listeners.Listeners_Boxed;

function main() {
	// Plain SAM — javac would accept the lambda directly.
	trace(Listeners.runOnClick(id -> Sys.println("click=" + id), 7));

	// SAM with default toString inherited from Object — still single abstract.
	trace(Listeners.runDescribe(v -> "v=" + v, 3));

	// Abstract equals re-declaration must be excluded from the count.
	trace(Listeners.runCompute(a -> a * a, 5));

	// default + static methods don't count toward abstractness.
	trace(Listeners.runTransform(x -> x + 1, 10));

	// Non-overloaded SAM with non-void return — sanity check.
	trace(Listeners.runMaker(n -> "made-" + n, 2));

	// SAM-aware overload disambiguation: distinct arg arities so the right
	// candidate is unambiguous.
	trace(Listeners.overloaded((id:Int) -> Sys.println("ovl-click=" + id), 1));
	trace(Listeners.overloaded((s:String) -> s + "!", "hi"));

	// A closure converted to OnClick must implement OnClick at runtime — but
	// NOT Unused, which is structurally identical yet never used as a
	// conversion target. Guards against closures promiscuously implementing
	// every matching SAM interface on the classpath.
	var cb:Listeners_OnClick = id -> Sys.println("bound-click=" + id);
	Listeners.runOnClick(cb, 99);
	trace(Std.isOfType(cb, Listeners_OnClick));
	trace(Std.isOfType(cb, Listeners_Unused));

	// Constructor-position SAM conversion with a bound instance-method
	// reference — the exact shape that crashed RideAssist's
	// `new TextToSpeech(this, onTtsInit)`. CtorOnly is never named anywhere
	// in user code (no typed local, no field, no import, no explicit cast).
	// The AST scan in genjvm.collect_used_functional_interfaces cannot
	// discover this conversion: a TNew has no callee subexpression, so the
	// constructor's parameter types (where CtorOnly's TInst lives) are never
	// visited — and the scan deliberately skips extern classes. Only
	// AbstractCast's writeback to functional_interfaces_used carries this
	// information from typing to codegen. If that writeback regresses, the
	// emitted closure won't implement CtorOnly and this `new` call will
	// ClassCastException at runtime.
	new Holder().run();

	// Generic SAM whose abstract method is `T invoke()` — same shape as
	// kotlin.jvm.functions.Function0<T>. The closure's typed invoke returns
	// Boxed (a specific class), but the FI bridge spawned for the erased
	// `Object invoke()` slot was forwarded to (meth.name, meth.dargs,
	// meth.dret) where meth.dret had already been declassified to Object — so
	// the bridge body invoked itself rather than the typed invoke, and the
	// later loop that would have emitted the proper Object→Boxed bridge was
	// short-circuited by has_method. Calling cb.invoke() from Java therefore
	// went into infinite recursion (StackOverflowError) instead of returning
	// the Boxed instance. RideAssist crash repro: a `() -> SyncStats` closure
	// passed to `new Thread(...)` against a classpath carrying both
	// kotlin Function0<SyncStats> and j.u.c.Callable<SyncStats>.
	trace(Listeners.runGenericInvoke(() -> new Listeners_Boxed(7)));
}

class Holder {
	public function new() {}
	public function run() {
		new Listeners_CtorSam(onCtor, 42);
	}
	function onCtor(n:Int):Void {
		Sys.println("ctor=" + n);
	}
}
