package cs.system.net.http;

/** A container for name/value tuples encoded using application/x-www-form-urlencoded MIME type. */
@:native("System.Net.Http.FormUrlEncodedContent")
extern class FormUrlEncodedContent extends cs.system.net.http.ByteArrayContent {
	function new(nameValueCollection:cs.system.collections.generic.IEnumerable<cs.system.collections.generic.KeyValuePair_2<String, String>>):Void;
}
