
package python;

import python.lib.Time;

class Sys {

	public static function time () {
		return Time.time()/1000;
	}

}