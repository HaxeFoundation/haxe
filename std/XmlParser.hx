class XmlParser {

	public static function parse( xmlData : String ) : Node {
		#flash
		untyped {
			var x = __new__(_global["XML"]);
			x.parseXML(xmlData);
			if( x.status != 0 )
				throw ("Xml parse error #"+x.status);
			return x;
		}
		#else error
		#end
	}

}