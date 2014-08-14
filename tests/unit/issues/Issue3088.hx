package unit.issues;

private class C {
	var x(get, null):String;
	function get_x() return "foo";
	function new() { }

	static public function getValue() {
		var m = new C();
		switch (m) {
			case {
				x: x = "foo"
			}:
				return x;
			case _:
				return null;
		}
	}
}

private typedef PublicationData = {
    year:Int,
    artist:String,
    name:String,
    type:String,
    ?tracks:Array<String>,
    ?bSide:String
}

private abstract Publication(PublicationData) from PublicationData {
    public var year(get, never):Int;
    public var artist(get, never):String;
    public var name(get, never):String;
    public var kind(get, never):PublicationKind;
    inline function get_year() return this.year;
    inline function get_artist() return this.artist;
    inline function get_name() return this.name;
    inline function get_kind()
    	return
            if (this.type == 'album')
                Album(this.tracks);
    		else
                Single(this.bSide);
}

private enum PublicationKind {
    Album(tracks:Array<String>);
    Single(bSide:String);
}

class Issue3088 extends Test {
	function test() {
		eq("foo", C.getValue());
	}

	function testCrazyJuraj() {
        var publications:Array<Publication> = [
            {
                year : 1967,
                artist : "The Jimi Hendrix Experience",
                name : "Are You Experienced?",
                type : "album",
                tracks : [/* ... */],
            },
            {
                year : 1978,
                artist : "Bob Marley and the Wailers",
                name : "Babylon by Bus",
                type : "album",
                tracks : [/* ... */],
            },
            {
                year : 1962,
                artist : "John Lee Hooker",
                name : "Boom Boom",
                type : "single",
                bSide : "Drug Store Woman",
            },
        ];
        var albumsBefore1970 = [];
     	for (p in publications)
            switch p {
                case {
             		year : _ < 1970 => true,
             		name : name,
             		kind : kind = Album(tracks)
            	}:
                    albumsBefore1970.push({
                        name : name,
                        kind : kind
                    });
                default:
            }

    	eq(1, albumsBefore1970.length);
    	eq("Are You Experienced?", albumsBefore1970[0].name);
	}
}