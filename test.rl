// enum structure
Maybe =
	Some [T]
| None

// structure
Incrementer = 
	Incrementer [name @ String, num @ Number, base_cost @ Number]

get_cost(incr: Incrementer) -> number
	=> pow(incr.base_cost * incr.num, 1.15)  

main() -> void
{
	incr = Incrementer [
		name @ "Spare Change",
		num  @  1,
		base_cost @ 1
	]
}
