class Base {
	var a: int
	var b: int
	func new(a: int, b: int) {
		this.a = a
		this.b = b
	}
	func print() {
		Main.printf("a: %d, b: %d\n", a, b)
	}
}
class Main extends Base {
	@LinkName("printf") @CallConv("vararg")	
	static extern func printf(v: String): void
	var c: int
	func new() {
		super(1, 2)
		c = 4
	}
	static func main(): int {
		var m = new Main()
		printf("Hello, world! %d, %d, %d\n", m.a, m.b, m.c)
		m.print()
		0
	}
}
