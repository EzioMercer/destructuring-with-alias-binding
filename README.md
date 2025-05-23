# Destructuring with Alias Binding

# Table of content:
+ [Motivations](#motivations)
+ [Possible solutions without this proposal](#possible-solutions-without-this-proposal)
  + [Rewrite with ...args](#rewrite-with-args)
  + [Rewrite arrow functions into classic functions](#rewrite-arrow-functions-into-classic-functions)
+ [Features](#features)
  + [Granular control of the passed object](#granular-control-of-the-passed-object)
  + [More easy way to create objects from an existing object](#more-easy-way-to-create-objects-from-an-existing-object)
+ [Improvements](#improvements)
  + [Grouping](#grouping)
+ [Similar possibilities in other languages](#similar-possibilities-in-other-languages)
  + [F#](#f-as-keyword)
  + [Haskell](#haskell--symbol)
  + [Erlang](#erlang--symbol)
+ [Notes](#notes)
+ [Links](#links)
  + [Discussions](#discussions)

# Motivations

This proposal suggests a new syntax which will be useful shorthand for saving destructed variables in object
immediately without the need to repeat them to create a new object

In my practice, it is ubiquitous to write something like this:

```js
const foo = (config) => {
    const _config = {
        ...config,
        x: Number(config.x)
    }
    
    console.log(_config);
    /*
    Output:
    {
        ...some properties,
        x: number
    }
    */
}
```

And here is the problem for me because `config` can be an object with extra keys which I don't want to have inside the
function.
So I have to rewrite it like this:

```js
const foo = ({
    prop1,
    prop2,
    x
}) => {
    const _config = {
        prop1,
        prop2,
        x: Number(x)
    }
    
    console.log(_config);
    /*
    Output:
    {
        prop1: any,
        prop2: any,
        x: number
    }
    */
}
```

And now the problem is that I have to write the exact same things twice.
So my proposal is to let rewrite the above code in that way:

```js
const foo = ({ prop1, prop2, x } as config) => {
    const _config = {
        ...config,
        x: Number(x)
    }
    
    console.log(_config);
    /*
    Output:
    {
        prop1: any,
        prop2: any,
        x: number
    }
    */
}
```

In that way I can access the `x` explicitly writing it or get it from `config` like this `config.x` or `config['x']`

# Possible solutions without this proposal

### Rewrite with `...args`

```js
const foo = (...args) => {
    const _config = {
        ...args[0],
        x: Number(x)
    }
    
    console.log(_config);
    /*
    Output:
    {
        ...some properties,
        x: number
    }
    */
}
```

But the problem with this approach is that now I don't have control of which properties will be passed into the first
argument.
With this proposal, at least I can easily omit the unnecessary properties which can cause bugs or unexpected
results

### Rewrite arrow functions into classic functions

```js
function foo({
    prop1,
    prop2,
    x
}) {
    const _config = {
        ...arguments[0],
        x: Number(x)
    }
    
    console.log(_config);
    /*
    Output:
    {
        prop1: any,
        prop2: any,
        x: number
    }
    */
}
```

This is the closest approach because now I can access properties both individually and by the alias using `arguments`

# Features

## Granular control of the passed object

Despite the approach using the `arguments` which is great for first level aliases it is still impossible to control
second or more levels of object.
But with this proposal we can control like this:

```js
const foo = ({
    prop1,
    prop2,
    prop3: {
        prop4,
        prop5
    } as innerConfig,
    prop6: {
        prop7: {
            prop8,
            prop9
        } as innerInnerInnerConfig
    } as innerInnerConfig,
    x
} as config) => {
    const _config = {
        ...config,
        x: Number(x)
    }
    
    console.log(_config);
    /*
    {
        prop1: any,
        prop2: any,
        prop3: {
            prop4: any,
            prop5: any
        },
        prop6: {
            prop7: {
                prop8: any,
                prop9: any
            }
        },
        x: number
    }
    */
    
    console.log(innerConfig);
    /*
    {
        prop4: any,
        prop5: any
    }
    */
    
    console.log(innerInnerConfig);
    /*
    {
        prop7: {
            prop8: any,
            prop9: any
        }
    }
    */
    
    console.log(innerInnerInnerConfig);
    /*
    {
        prop8: any,
        prop9: any
    }
    */
}
```

## More easy way to create objects from an existing object

```js
const foo = {
    a: 1,
    b: 2,
    c: {
        d: 3,
        e: 4
    },
    f: {
        g: {
            h: 5,
            i: 6
        },
        j: 7,
        k: 8
    }
}

const { a, b } as first = foo;
const { c: { d, e } } as second = foo;
const { f: { g, j, k } } as third = foo;
const { f: { g: { h, i } } } as fourth = foo;

// Or combine them
const { a, b } as first = foo;
const {
    c: {
        d,
        e
    } as second,
    f: {
        g: {
            h,
            i
        } as fourth,
        j,
        k
    } as third
} = foo

console.log(first);
/*
{
    a: 1,
    b: 2
}
*/

console.log(second);
/*
{
    d: 3,
    e: 4
}
*/


console.log(third);
/*
{
    g: {
        h: 5,
        i: 6
    },
    j: 7,
    k: 8
}
*/

console.log(fourth);
/*
{
    h: 5,
    i: 6
}
*/
```

# Improvements

## Grouping

As you can see from [this example,](#more-easy-way-to-create-objects-from-an-existing-object) we can't combine the first
level properties into a big one because they are directly inside an object.
But if we allow grouping syntax, then it is easy to combine them too.
For example, we can use object syntax for grouping:

```js
const { a, b } as first = foo;
const { c: { d, e } } as second = foo;
const { f: { g, j, k } } as third = foo;
const { f: { g: { h, i } } } as fourth = foo;

// We can rewrite it to:

const {
    {
        a,
        b
    } as first,
    c: {
        d,
        e
    } as second,
    f: {
        g: {
            h,
            i
        } as fourth,
        j,
        k
    } as third
} = foo
```

Also, it can be useful to grouping properties from different levels of depths:

```js
const foo = {
    a: 1,
    b: 2,
    c: {
        d: 3,
        e: 4
    },
    f: {
        g: {
            h: 5,
            i: 6
        },
        j: 7,
        k: 8
    }
}

const {
    {
        a,
        b,
        c: {
            d,
            e
        }
    } as first,
    {
      c: {
          d,
          e
      },
      f: {
          j,
          k
      } as third
    } as second
}

console.log(first);
/*
{
    a: 1,
    b: 2,
    c: {
        d: 3,
        e: 4
    }
}
*/

console.log(second);
/*
{
    c: {
        d: 3,
        e: 4
    },
    f: {
        j: 7,
        k: 8
    }
}
*/

console.log(third);
/*
{
    j: 7,
    k: 8
}
*/
```

# Similar possibilities in other languages

### F# (`as` keyword)

```f#
let (var1, var2) as tuple = (1, 2)

printfn "var1: %d var2: %d tuple: %A" var1 var2 tuple // Output: var1: 1 var2: 2 tuple: 1,2
```

### Haskell (`@` symbol)

```haskell
processTuple :: (Int, Int) -> String
processTuple t@(a, b) = "First: " ++ show a ++ ", Second: " ++ show b ++ ", Original Tuple: " ++ show t

main :: IO ()
main = do
  putStrLn $ processTuple (1, 2) -- Output: First: 1, Second: 2, Original Tuple: (1,2)
```

```haskell
data Person = Person { name :: String, age :: Int } deriving Show

processPerson :: Person -> String
processPerson p@(Person n a) = "Name: " ++ n ++ ", Age: " ++ show a ++ ", Original Person: " ++ show p

main :: IO ()
main = do
  let alice = Person { name = "Alice", age = 30 }
  putStrLn $ processPerson alice -- Output: Name: Alice, Age: 30, Original Person: Person {name = "Alice", age = 30}
```

### Erlang (`=` symbol)

```erlang
process_tuple(T={A, B}) ->
    io:format("First: ~p, Second: ~p, Original Tuple: ~p~n", [A, B, T]). % Output: First: 1, Second: 2, Original Tuple: {1,2}

process_person(P=#{name := N, age := Ag}) ->
    io:format("Name: ~s, Age: ~p, Original Person: ~p~n", [N, Ag, P]). % Output: Name: Alice, Age: 30, Original Person: #{age => 30,name => "Alice"}

main(_) ->
    process_tuple({1, 2}),
    process_person(#{name => "Alice", age => 30}).
```

# Notes

I suggest `as` keyword because it is the first word which I came up with.
As we can see, other languages use different syntax for the same thing.
My suggestion doesn't require the `as` keyword as the solution.
It can be:
+ keyword: `in`, `into`, `use`, `save`, `template`, `like` etc.
+ symbol/operator: `=>`, `>>`, `->`, `<=`, `<<`, `<-`, `@`, `#`, `$`, `~`, `~>`, `<~` etc.

The keyword or symbol or operator also is not required to come after the destructing syntax

# Links

## Discussions

+ [Destructuring with Alias Binding](https://es.discourse.group/t/destructuring-with-alias-binding/2370)

---

### P.S.
Please don't hesitate to:

+ give a star ☆
+ share your ideas
+ share this proposal with others
+ provide more useful examples
+ provide possible problems with this proposal
+ suggest text improvements
+ help with `emu` file


You can browse the [ecmarkup output](https://EzioMercer.github.io/destructuring-with-alias-binding/)
or browse the [source](https://github.com/EzioMercer/destructuring-with-alias-binding/blob/main/spec.emu)
