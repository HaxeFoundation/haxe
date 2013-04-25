pub trait HxEnum {
	}
pub trait HxObject {
	}
impl<T> HxObject for BaseIter<T> {
	}
impl HxObject for Clone {
	}
impl HxObject for os::Pipe {
	}
impl HxObject for path::Path {
	}
impl HxObject for io::ReaderUtil {
	}
impl HxObject for io::Reader {
	}
impl HxObject for io::Writer {
	}
