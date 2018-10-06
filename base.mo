class Base {
	var a: int = 12
	var b: int = 13
	func new() {

	}
	virtual func print() {
		Main.printf("a: %d, b: %d\n", a, b)
	}
}
class Main extends Base {
	@LinkName("printf") @CallConv("vararg")	
	static extern func printf(v: String): void
	var c: int = 4
	func new() {
		super()
	}
	override func print() {
		printf("Hello, world: %d, %d\n", a, b)
	}
	static func main(): int {
		val ma: Main = new Main()
		val ba: Base = ma as Base
		ba.print()
		0
	}
}
