package flash.media;

@:final extern class ID3Info implements Dynamic {
	var album : String;
	var artist : String;
	var comment : String;
	var genre : String;
	var songName : String;
	var track : String;
	var year : String;
	function new() : Void;
}
