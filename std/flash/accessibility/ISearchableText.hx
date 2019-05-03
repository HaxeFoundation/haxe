package flash.accessibility;

@:require(flash10_1) extern interface ISearchableText {
	var searchText(get,never) : String;
	private function get_searchText() : String;
}
