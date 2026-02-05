package cs.system.net;

/** Provides data for the  event. */
@:native("System.Net.OpenWriteCompletedEventArgs")
extern class OpenWriteCompletedEventArgs extends cs.system.componentmodel.AsyncCompletedEventArgs {
	/**
	 * Gets a writable stream that is used to send data to a server.
	 * @return A  where you can write data to be uploaded.
	 */
	var Result(default, never):cs.system.io.Stream;
}
