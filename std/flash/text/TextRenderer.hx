package flash.text;

#if !flash8
"This class is only accesible in Flash8"
#end

extern class TextRenderer {

	static var maxLevel : Float;
	static function setAdvancedAntialiasingTable( fontName : String, fontStyle: String, colorType : String, advancedAntialiasingTable : Array<Dynamic> ) : Void;

}