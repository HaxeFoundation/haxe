
package python;

class Tools {

	

	public static function substring( s:String, startIndex : Int, ?endIndex : Int ) : String {
		if (startIndex < 0) startIndex = 0;
		if (endIndex == null) {
			return untyped __python__("s[startIndex:]");		
		} else {
			if (endIndex < 0) endIndex = 0;
			if (endIndex < startIndex) {
				
				return untyped __python__("s[endIndex:startIndex]");
			} else {
				
				return untyped __python__("s[startIndex:endIndex]");
			}
			
		}
	}	

	public static function substr( s:String, startIndex : Int, ?len : Int ) : String {
		if (len == null) {
			return untyped __python__("s[startIndex:]");
		} else {
			if (len == 0) return "";
			return untyped __python__("s[startIndex:startIndex+len]");
		}
		
	}


}