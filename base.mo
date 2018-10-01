import std.collections.HashMap

class Main {
	@LinkName("llvm.sqrt.f32")
	static extern func sqrt(v: float): float
	@CallConv("c")
	static func fib(n: int): int
		if (n < 2) n else ( fib(n - 1) + fib(n - 2))
	static func baz(v: bool): int
		if (v) { 23 } else { 5 }
	static func main(): int {
			printf("%d\n", fib(5))
		var i = 0
		while((i += 1) < 10) {
			if (i == 2) continue
			printf("%d\n", i)
		}
		0
	}
	@LinkName("printf") @CallConv("vararg")	
	static extern func printf(v: String): void
}