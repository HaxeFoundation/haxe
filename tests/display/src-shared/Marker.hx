class Marker {
	static var markerRe = ~/{-(\d+)-}/g;

	static public function extractMarkers(doc:String) {
		var markers = #if macro [] #else new Map() #end;
		var posAcc = 0;
		var src = markerRe.map(doc, function(r) {
			var p = r.matchedPos();
			var name = r.matched(1);
			var pos = p.pos - posAcc;
			posAcc += p.len;
			#if macro
			markers.push(macro $v{Std.parseInt(name)} => $v{pos});
			#else
			markers.set(Std.parseInt(name), pos);
			#end
			return "";
		});
		return {source: src, markers: markers};
	}
}
