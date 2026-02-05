package cs.system.net;

/** Provides data for the  event. */
@:native("System.Net.OpenReadCompletedEventArgs")
extern class OpenReadCompletedEventArgs extends cs.system.componentmodel.AsyncCompletedEventArgs {
	/**
	 * Gets a readable stream that contains data downloaded by a  method.
	 * @return A  that contains the downloaded data.
	 */
	var Result(default, never):cs.system.io.Stream;
}
