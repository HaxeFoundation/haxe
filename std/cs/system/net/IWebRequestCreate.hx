package cs.system.net;

/** Provides the base interface for creating  instances. */
@:native("System.Net.IWebRequestCreate")
extern interface IWebRequestCreate {
	/**
	 * Creates a  instance.
	 * @param uri The uniform resource identifier (URI) of the Web resource.
	 * @return A  instance.
	 */
	function Create(uri:cs.system.Uri):cs.system.net.WebRequest;
}
