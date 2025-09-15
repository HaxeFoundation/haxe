package hxcoro.ds;

import haxe.ds.Vector;
import haxe.Exception;
import hxcoro.ds.Out;

class Page<T> {
	public final data:Vector<T>;
	public var numDeleted:Int;
	public var next:Null<Page<T>>;

	public function new(size:Int) {
		this.data = new Vector(size);
		numDeleted = 0;
	}

	function removeFrom(element:T, startIndex:Int) {
		for (i in startIndex...data.length - numDeleted) {
			if (data[i] == element) {
				blitAt(i);
				return true;
			}
		}
		return false;
	}

	public function reset() {
		numDeleted = 0;
		next = null;
	}

	public inline function freeSpace() {
		return data.length - numDeleted;
	}

	function blitAt(index:Int) {
		final toBlit = freeSpace() - index - 1;
		if (toBlit > 0) {
			Vector.blit(data, index + 1, data, index, toBlit);
		}
		numDeleted++;
	}
}

class PagedDeque<T> {
	final vectorSize:Int;
	var currentPage:Page<T>;
	var currentIndex:Int;
	var lastPage:Page<T>;
	public var lastIndex(default, null):Int;

	public function new(vectorSize = 8) {
		this.vectorSize = vectorSize;
		currentPage = new Page(vectorSize);
		currentIndex = 0;
		lastPage = currentPage;
		lastIndex = 0;
	}

	inline function getPageDataAt<T>(page:Page<T>, index:Int) {
		return page.data[index];
	}

	inline function setPageDataAt<T>(page:Page<T>, index:Int, value:T) {
		page.data[index - page.numDeleted] = value;
	}

	public function forEach(f:T->Void) {
		var currentPage = currentPage;
		var currentIndex = currentIndex;
		while (currentPage != lastPage) {
			while (currentIndex < currentPage.freeSpace()) {
				f(getPageDataAt(currentPage, currentIndex++));
			}
			currentIndex = 0;
			currentPage = currentPage.next;
		}
		while (currentIndex < lastIndex - currentPage.numDeleted) {
			f(getPageDataAt(currentPage, currentIndex++));
		}
	}

	public function mapInPlace(f:T->T) {
		var currentPage = currentPage;
		var currentIndex = currentIndex;
		while (currentPage != lastPage) {
			while (currentIndex < currentPage.freeSpace()) {
				setPageDataAt(currentPage, currentIndex, f(getPageDataAt(currentPage, currentIndex++)));
			}
			currentIndex = 0;
			currentPage = currentPage.next;
		}
		while (currentIndex < lastIndex) {
			setPageDataAt(currentPage, currentIndex, f(getPageDataAt(currentPage, currentIndex++)));
		}
	}

	public function fold<A>(acc:A, f:(acc:A, elt:T) -> A) {
		var currentPage = currentPage;
		var currentIndex = currentIndex;
		while (currentPage != lastPage) {
			while (currentIndex < currentPage.freeSpace()) {
				acc = f(acc, getPageDataAt(currentPage, currentIndex++));
			}
			currentIndex = 0;
			currentPage = currentPage.next;
		}
		while (currentIndex < lastIndex) {
			acc = f(acc, getPageDataAt(currentPage, currentIndex++));
		}
		return acc;
	}

	public function push(x:T) {
		if (lastIndex == lastPage.freeSpace()) {
			// current page is full
			if (lastPage.next == null) {
				// we have no next page, allocate one
				lastPage.next = new Page(vectorSize);
			}
			lastPage = lastPage.next;
			lastPage.next = null;
			lastIndex = 1;
			setPageDataAt(lastPage, 0, x);
			return lastPage;
		}
		setPageDataAt(lastPage, lastIndex++, x);
		return lastPage;
	}

	public function pop() {
		if (currentIndex == currentPage.freeSpace()) {
			// end of page, need to swap
			var nextPage = currentPage.next;
			if (nextPage == null) {
				throw new Exception("pop() was called on empty PagedDeque");
			}
			if (lastPage.next == null) {
				// reuse current page as next last page
				lastPage.next = currentPage;
				currentPage.next = null;
				currentPage.reset();
			}
			currentPage = nextPage;
			currentIndex = 1;
			return getPageDataAt(currentPage, 0);
		} else if (currentIndex == currentPage.freeSpace() - 1 && currentPage.next == null) {
			// deque is empty, reset to reuse current page
			resetCurrent();
			return getPageDataAt(currentPage, currentPage.freeSpace() - 1);
		} else {
			return getPageDataAt(currentPage, currentIndex++);
		}
	}

	public function remove(page:Page<T>, element:T) {
		return if (page == currentPage) {
			@:privateAccess page.removeFrom(element, currentIndex);
		} else {
			@:privateAccess page.removeFrom(element, 0);
		}
	}

	public function tryPop(out:Out<T>) {
		if (isEmpty()) {
			// TODO: could probably integrate this better in the branches below
			return false;
		}
		if (currentIndex == vectorSize) {
			// end of page, need to swap
			var nextPage = currentPage.next;
			if (lastPage.next == null) {
				// reuse current page as next last page
				lastPage.next = currentPage;
				currentPage.next = null;
			}
			currentPage = nextPage;
			currentIndex = 1;
			out.set(currentPage.data[0]);
			return true;
		} else if (currentIndex == vectorSize - 1 && currentPage.next == null) {
			// deque is empty, reset to reuse current page
			currentIndex = 0;
			lastIndex = 0;
			out.set(currentPage.data[vectorSize - 1]);
			return true;
		} else {
			out.set(currentPage.data[currentIndex++]);
			return true;
		}
	}

	public function tryPeek(out:Out<T>) {
		if (isEmpty()) {
			return false;
		}

		out.set(getPageDataAt(lastPage, lastIndex - 1));

		return true;
	}

	public function isEmpty() {
		while (currentIndex == currentPage.freeSpace()) {
			if (currentPage.next == null || currentPage == lastPage) {
				resetCurrent();
				return true;
			}
			currentPage = currentPage.next;
			currentIndex = 0;
		}

		return currentPage == lastPage && currentIndex == lastIndex - currentPage.numDeleted;
	}

	function resetCurrent() {
		currentIndex = 0;
		lastIndex = 0;
		currentPage.reset();
	}
}
