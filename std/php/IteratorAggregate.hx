package php;

extern interface IteratorAggregate<T> {
	/** 
		This method is not public to not induce haXe users to use it ;) 
		Use iterator() instead.
		The return type would be Aggregator that is unusable in haXe 
	**/
	private function getIterator() : Iterator<T>; // 
}