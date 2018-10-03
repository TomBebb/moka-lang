class Base {
	var a: int = 12
	var b: int = 13
	func new() {
	}
	func print() {
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
	static func main(): int {
		var m = new Main()
		printf("Hello, world! %d, %d, %d\n", m.a, m.b, m.c)
		m.print()
		0
	}
}
