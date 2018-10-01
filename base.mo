import std.collections.HashMap

class Main {
	static func main(): int {
		printf("Hello, world!")
		0
	}
	@LinkName("printf") @CallConv("vararg")	
	static extern func printf(v: String): void
}