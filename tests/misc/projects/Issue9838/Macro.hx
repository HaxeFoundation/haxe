import haxe.macro.Context;
import haxe.io.Bytes;

class Macro {
	public static macro function include() {
		Context.addResource("foo", Bytes.ofString("hi"));
		if(!Std.isOfType(Context.getResources()["foo"], Bytes)) {
			Context.error('Invalid resource encoding/decoding', (macro {}).pos);
		}
		return macro {};
	}
}