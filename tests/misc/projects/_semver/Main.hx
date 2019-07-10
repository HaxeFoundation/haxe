class Main {
	public static function main():Void {
		// 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0

		#if (version("1.0.0-alpha") < version("1.0.0-alpha.1"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-alpha.1") < version("1.0.0-alpha.beta"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-alpha.beta") < version("1.0.0-beta"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-beta") < version("1.0.0-beta.2"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-beta.2") < version("1.0.0-beta.11"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-beta.11") < version("1.0.0-rc.1"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-rc.1") < version("1.0.0"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-rc.1") < version("1.0.1"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-rc.1") < version("1.1.0"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-rc.1") < version("2.0.0"))
		#else #error "Failed semver comparison" #end

		#if (version("1.0.0-rc.1") == version("1.0.0-rc.1"))
		#else #error "Failed semver comparison" #end
	}
}