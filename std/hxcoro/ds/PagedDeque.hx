package hxcoro.ds;

import haxe.ds.Vector;
import haxe.Exception;

private class Page<T> {
	public final data:Vector<T>;
	public var next:Null<Page<T>>;

	public function new(size:Int) {
		this.data = new Vector(size);
	}
}

class PagedDeque<T> {
	final vectorSize:Int;
	var currentPage:Page<T>;
	var currentIndex:Int;
	var lastPage:Page<T>;
	var lastIndex:Int;

	public function new(vectorSize = 8) {
		this.vectorSize = vectorSize;
		currentPage = new Page(vectorSize);
		currentIndex = 0;
		lastPage = currentPage;
		lastIndex = 0;
	}

	public function push(x:T) {
		if (lastIndex == vectorSize) {
			// current page is full
			if (lastPage.next == null) {
				// we have no next page, allocate one
				lastPage.next = new Page(vectorSize);
			}
			lastPage = lastPage.next;
			lastPage.next = null;
			lastIndex = 1;
			lastPage.data[0] = x;
			return;
		}
		lastPage.data[lastIndex++] = x;
	}

	public function pop() {
		if (currentIndex == vectorSize) {
			// end of page, need to swap
			var nextPage = currentPage.next;
			if (nextPage == null) {
				throw new Exception("pop() was called on empty PagedDeque");
			}
			if (lastPage.next == null) {
				// reuse current page as next last page
				lastPage.next = currentPage;
				currentPage.next = null;
			}
			currentPage = nextPage;
			currentIndex = 1;
			return currentPage.data[0];
		} else if (currentIndex == vectorSize - 1 && currentPage.next == null) {
			// deque is empty, reset to reuse current page
			currentIndex = 0;
			lastIndex = 0;
			return currentPage.data[vectorSize - 1];
		} else {
			return currentPage.data[currentIndex++];
		}
	}

	public function isEmpty() {
		return currentPage.data == lastPage.data && currentIndex == lastIndex;
	}
}
