function fp32( f : Float ) : Float {
	return haxe.io.FPHelper.i32ToFloat(haxe.io.FPHelper.floatToI32(f));
}

fp32( -1.3 ) == -1.2999999523162842;
fp32( 15.999999 ) == 15.999999046325684;
fp32( 15.999999999 ) == 16;
fp32( 16 ) == 16;
fp32( 16.000000001 ) == 16;
fp32( 16.000001 ) == 16.000001907348633;

