import std.collections.HashMap

class Main {
	@LinkName("llvm.sqrt.f32")
	static extern func sqrt(v: float): float
	static func main(): int {
		var i = 0
		while((i += 1) < 10) {
			if i == 2 { continue }
			printf("%d\n", i)
		}
		0
	}
	@LinkName("printf") @CallConv("vararg")	
	static extern func printf(v: String): void
}