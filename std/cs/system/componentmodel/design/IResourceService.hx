package cs.system.componentmodel.design;

/** Provides an interface for designers to access resource readers and writers for specific  resource types. */
@:native("System.ComponentModel.Design.IResourceService")
extern interface IResourceService {
	/**
	 * Locates the resource reader for the specified culture and returns it.
	 * @param info The  of the resource for which to retrieve a resource reader.
	 * @return An  interface that contains the resources for the culture, or  if no
	 * resources for the culture exist.
	 */
	function GetResourceReader(info:cs.system.globalization.CultureInfo):cs.system.resources.IResourceReader;
	/**
	 * Locates the resource writer for the specified culture and returns it.
	 * @param info The  of the resource for which to create a resource writer.
	 * @return An  interface for the specified culture.
	 */
	function GetResourceWriter(info:cs.system.globalization.CultureInfo):cs.system.resources.IResourceWriter;
}
