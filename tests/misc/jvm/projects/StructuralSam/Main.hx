import test.Listeners;
import test.Listeners.Listeners_OnClick;
import test.Listeners.Listeners_WithToString;
import test.Listeners.Listeners_AbstractEqualsPlusOne;
import test.Listeners.Listeners_WithDefaults;
import test.Listeners.Listeners_StringMaker;
import test.Listeners.Listeners_Unused;

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
}
