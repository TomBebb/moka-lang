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
	static func printBool(b: bool): void {
		printf(if b "true\n" else "false\n")
	}
	static func main(): int {
		var m = new Main()
		val c = m.c
		var tup  = (1, false, true, c)
		printf("Hello, world! %d, %d, %d\n", m.a, m.b, c)
		printf("Tuple: %d, %d\n", tup[0], tup[3])
		printBool(false)
		printBool(true)
		printBool(tup[1])
		printBool(tup[2])
		m.print()
		0
	}
}
