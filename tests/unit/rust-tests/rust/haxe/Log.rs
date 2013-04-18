use rust::Lib;
mod HxObject;
pub struct Log;
pub impl Log {trace: Option<@@fn(@HxObject, @HxObject)->()>;
}
impl HxObject for haxe::Log {
}
