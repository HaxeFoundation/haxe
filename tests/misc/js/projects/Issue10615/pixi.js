export class Application {
	constructor(options) {}
	test() {
		return "test";
	}
	static name() {
		return "Application";
	}
}

export class Assets {
	static load(url) {
		return url;
	}
}

export default class {
	static name() {
		return "default name";
	}
};
