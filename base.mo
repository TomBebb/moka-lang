
class Base {
	var a: int
	var b: int
	func new(a: int, b: int) {

		this.a = a
		this.b = b
	}
}
class Main extends Base {
	@LinkName("printf") @CallConv("vararg")	
	static extern func printf(v: String): void
	var c: int
	func new() {
		a = 2
		b = 3
	}
	static func main(): int {
		var m = new Main()
		printf("Hello, world! %d, %d\n", m.a, m.b)
		0
	}
}
