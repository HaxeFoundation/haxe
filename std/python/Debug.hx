
package python;

import python.lib.Inspect;
import python.lib.PPrint;

class Debug {

	public static function dump (x:Dynamic) {
		PPrint.pprint(Inspect.getmembers(x));
	}

}