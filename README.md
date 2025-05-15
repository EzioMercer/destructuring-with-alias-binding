# Naming destructed variables

---

This proposal suggests a new syntax which will be useful shorthand for saving destructed variables in object
immediately without the need to repeat them to create a new object

In my practice, it is ubiquitous to write something like this:

```js
function foo(config) {
	const _config = {
		...config,
		x: Number(config.x)
	}
	
	console.log(_config);
}
```

And here is the problem for me because `config` can be an object with extra keys which I don't want to have inside the
function.
So I have to rewrite it like this:

```js
function foo({ prop1, prop2, ..., propN, x }) {
	const _config = {
		prop1,
		prop2,
		...,
		propN,
		x: Number(x)
	}
	
	console.log(_config);
}
```

And now the problem is that I have to write the exact same things twice.
So my proposal is to let rewrite the above code in that way:

```js
function foo({ prop1, prop2, ..., propN, x } as config) {
	const _config = {
		...config,
		x: Number(x)
	}
	
	console.log(_config);
}
```

In that way I can access the `x` explicitly writing it or get it from `config` like this `config.x` or `config['x']`

---

# Similar possibilities in other languages:

## F# (`as` keyword)

```f#
let (var1, var2) as tuple = (1, 2)

printfn "var1: %d var2: %d tuple: %A" var1 var2 tuple // Output: var1: 1 var2: 2 tuple: 1,2
```

## Haskell (`@` symbol)

```haskell
processTuple :: (Int, Int) -> String
processTuple t@(a, b) = "First: " ++ show a ++ ", Second: " ++ show b ++ ", Original Tuple: " ++ show t

main :: IO ()
main = do
  putStrLn $ processTuple (1, 2) -- Output: First: 1, Second: 2, Original Tuple: (1,2)
```

```haskel
data Person = Person { name :: String, age :: Int } deriving Show

processPerson :: Person -> String
processPerson p@(Person n a) = "Name: " ++ n ++ ", Age: " ++ show a ++ ", Original Person: " ++ show p

main :: IO ()
main = do
  let alice = Person { name = "Alice", age = 30 }
  putStrLn $ processPerson alice -- Output: Name: Alice, Age: 30, Original Person: Person {name = "Alice", age = 30}
```

## Erlang (`=` symbol)

```erlang
process_tuple(T={A, B}) ->
    io:format("First: ~p, Second: ~p, Original Tuple: ~p~n", [A, B, T]). % Output: First: 1, Second: 2, Original Tuple: {1,2}

process_person(P=#{name := N, age := Ag}) ->
    io:format("Name: ~s, Age: ~p, Original Person: ~p~n", [N, Ag, P]). % Output: Name: Alice, Age: 30, Original Person: #{age => 30,name => "Alice"}

main(_) ->
    process_tuple({1, 2}),
    process_person(#{name => "Alice", age => 30}).
```

You can browse the [ecmarkup output](https://EzioMercer.github.io/naming-destructed-variables/)
or browse the [source](https://github.com/EzioMercer/naming-destructed-variables/blob/main/spec.emu)
